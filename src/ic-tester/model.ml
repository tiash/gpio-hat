open Core

module Pin_spec = Pins.Expert.Spec


let pin_mark pin =
  match Pin_spec.kind pin with
  | Input -> ("-->", "<--")
  | Output -> ("<--", "-->")
  | Input_output -> ("<->", "<->")
  | Constant true -> ("^--", "--^")
  | Constant false -> ("_--", "--_")
  | Not_connected -> (" x-", "-x ")

let left_pin_mark pin = fst (pin_mark pin)

let right_pin_mark pin = snd (pin_mark pin)

type t = {
  name : string;
  summary : string;
  aliases : string list;
  description : string;
  pins : Pin_spec.t list;
  logic : (unit Logic.t[@sexp.opaque]);
}
[@@deriving sexp_of, fields]

let create name ?(aliases = []) ~summary ~description spec =
  { name; summary; aliases; description; pins = Pins.Expert.pins spec; logic = Pins.Expert.value spec }

let to_string t = sprintf "%s - %s" t.name t.summary

let to_string_pinout t =
  let height =
    List.fold ~init:0 t.pins ~f:(fun height pin ->
        match Pin_spec.pin pin with A n | B n -> Int.max height n)
  in
  let pins =
    List.init height ~f:(fun n ->
        ( List.find t.pins ~f:(fun pin ->
              match Pin_spec.pin pin with
              | A n' -> Int.equal n' (n + 1)
              | B _ -> false),
          List.find t.pins ~f:(fun pin ->
              match Pin_spec.pin pin with
              | B n' -> Int.equal n' (n + 1)
              | A _ -> false) ))
  in
  let label_width =
    List.fold t.pins ~init:1 ~f:(fun len pin ->
        Int.max len (String.length (Pin_spec.name pin)))
  in
  [ sprintf "%*s     ___ ___     %-*s" label_width "" label_width "" ]
  @ List.mapi pins ~f:(fun i (a, b) ->
        sprintf "%*s %s|%-3d%c%3d|%s %-*s" label_width
          (Option.value_map a ~f:Pin_spec.name ~default:"")
          (Option.value_map a ~f:left_pin_mark ~default:"   ")
          (i + 1)
          (if i = 0 then 'U' else ' ')
          ((2 * List.length pins) - i)
          (Option.value_map b ~f:right_pin_mark ~default:"   ")
          label_width
          (Option.value_map b ~f:Pin_spec.name ~default:""))
  @ [ sprintf "%*s     -------     %-*s" label_width "" label_width "" ]
  |> List.map ~f:String.rstrip
|> String.concat ~sep:"\n"

let truth_table _t _pins = raise_s [%message "TODO"]
