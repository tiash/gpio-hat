open Core

module Pin_model = struct
  module Kind = struct
type t = 
| Input |Output
| Bidirectional
| Constant of bool
| Not_connected
[@@deriving sexp_of, equal]
end
type t = { name : string; kind : Kind.t; pin : Gpio_hat.Pin.t } [@@deriving sexp_of, equal, fields]
let merge ~key:pin a b =
  assert (Gpio_hat.Pin.equal pin a.pin);
  assert (Gpio_hat.Pin.equal pin b.pin);
  if equal a b then a
else
  if not (String.equal a.name b.name) then
     raise_s [%message "Pin names don't match" (pin:Gpio_hat.Pin.t) (a:t) (b:t)]
else if not (Kind.equal a.kind b.kind) then
   raise_s [%message "Pin kinds not compatible" (pin:Gpio_hat.Pin.t) (a:t) (b:t)]
else a
end
module Let_syntax = struct
  type 'a t = { pins : Pin_model.t Gpio_hat.Pin.Map.t; value : 'a } [@@deriving sexp_of]

  let map {pins; value} ~f = { pins;value = f value }
  let both a b =
    { pins = Map.merge_skewed a.pins b.pins ~combine:Pin_model.merge
; value = (a.value, b.value) }

module Open_on_rhs = struct
  let create name pin kind value =
  { pins = Gpio_hat.Pin.Map.singleton pin {Pin_model.name; kind; pin}; value }

  let nc pin = create "NC" pin Not_connected ()
  let constant name pin value = create name pin (Constant value) ()
  let vcc pin = constant "VCC" pin true
  let gnd pin = constant "GND" pin false

  let input name pin =
create name pin Input (Ic_monad.Expert.input pin)
  let input' name pin =
create name pin Input (Ic_monad.Expert.input' pin)
  let output name pin =
create name pin Output (Ic_monad.Expert.output pin)
  let input_output name pin =
create name pin Bidirectional
((Ic_monad.Expert.input pin),
(Ic_monad.Expert.output pin))
  let input_output' name pin =
create name pin Bidirectional
((Ic_monad.Expert.input' pin),
(Ic_monad.Expert.output pin))
end 
end

type t = 
  { name : string
; summary : string
; aliases : string list
; description : string
; pins : Pin_model.t Gpio_hat.Pin.Map.t
; logic : (unit Ic_monad.t [@sexp.opaque])
}
[@@deriving sexp_of, fields]

let create ~name ?(aliases=[]) ~summary ~description { Let_syntax.pins; value=logic } =
(* CR mhorn: Check that we map all the necessary pins *)
  { name; summary; aliases; description; pins; logic }


let to_string t = sprintf "%s - %s" t.name t.summary

let pins t = Map.map t.pins ~f:(fun t -> t.name)

let to_string_pinout t = 
   let pins = List.init 40 ~f:(fun n ->
      Map.find t.pins (Gpio_hat.Pin.of_string (sprintf "A%d" n)), Map.find t.pins (Gpio_hat.Pin.of_string (sprintf "B%d" n)))
|> List.rev |> List.drop_while ~f:(fun (left,right) ->
  Option.is_none left && Option.is_none right)
|> List.rev
in
let l = Map.fold t.pins ~init:10 ~f:(fun ~key:_ ~data:t len -> Int.max len (String.length t.name)) in
([ sprintf "%*s   __ __   %-*s" l "" l "" ]
@
List.mapi pins ~f:(fun i (a,b) ->
sprintf "%*s %c|%-2d%c%2d|%c %-*s\n" l
(Option.value_map a ~f:Pin_model.name ~default:"")
(if Option.is_some a  then '-' else ' ')
(i+1)
(if i = 0 then 'U' else ' ')
(2 * List.length pins - i)
(if Option.is_some b  then '-' else ' ')
l
(Option.value_map b ~f:Pin_model.name ~default:""))
@ [ sprintf "%*s   -----   %-*s\n" l "" l "" ])
|> String.concat ~sep:"\n"
;;

let truth_table _t _pins =
raise_s [%message "TODO"]

module type S = sig
  val model : t
end
