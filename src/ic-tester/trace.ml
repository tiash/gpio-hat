open Core

  module Pin = struct
    type t =
      | Not_connected
| Constant of bool
      | Input of bool
      | Output of bool
      | Mismatch of bool
    [@@deriving sexp_of, compare]

    let of_action : Ic_monad.Action.t -> t = function
      | Input t -> Input t
      | Output t -> Output t
      | Constant t -> Constant t
      | Not_connected -> Not_connected

    let to_string_ansi ?(prev = Not_connected) t =
      match (t, prev) with
      | Not_connected, _ -> "\027[90;40m-\027[0m"
      | Constant false,_ ->           "\027[37;40m0\027[0m"
      | Constant true,_ ->            "\027[37;40m1\027[0m"
      | Input false, Input false -> "\027[36;40m0\027[0m"
      | Input false, _ -> "\027[30;46m0\027[0m"
      | Input true, Input true -> "\027[36;40m1\027[0m"
      | Input true, _ -> "\027[30;46m1\027[0m"
      | Output false, Output false -> "\027[32;40m0\027[0m"
      | Output false, _ -> "\027[30;42m0\027[0m"
      | Output true, Output true -> "\027[32;40m1\027[0m"
      | Output true, _ -> "\027[30;42m1\027[0m"
      | Mismatch false, _ -> "\027[30;41m0\027[0m"
      | Mismatch true, _ -> "\027[30;41m1\027[0m"
  end

  module State = struct
    type t = Pin.t Gpio_hat.Pin.Map.t [@@deriving sexp_of, compare]

    let row_a, row_b =
      List.partition_tf Gpio_hat.Pin.all ~f:(function A _ -> true | B _ -> false)

    let to_string_ansi' fmt ?(prev = Gpio_hat.Pin.Map.empty) t =
      let row pins =
        List.map pins ~f:(fun pin ->
            Pin.to_string_ansi ?prev:(Map.find prev pin)
              (Map.find t pin |> Option.value ~default:Pin.Not_connected))
        |> String.concat
      in
      fmt (row row_a) (row row_b)

    let to_string_ansi ?prev t = to_string_ansi' (sprintf "%s\n%s") ?prev t

    let to_string_ix_ansi ~ix ?prev t =
      to_string_ansi' (sprintf "%2d: %s\n    %s" ix) ?prev t
  end

  type t = State.t list [@@deriving sexp_of, compare]

  let to_string_ansi =
    let rec loop ~ix ?prev ~acc = function
      | [] -> List.rev acc |> String.concat ~sep:"\n"
      | hd :: tl ->
          let acc = State.to_string_ix_ansi ~ix ?prev hd :: acc in
          loop ~ix:(ix + 1) ~prev:hd ~acc tl
    in
    loop ~ix:1 ?prev:None ~acc:[]

let rec traces' ~prev_pins t =
  Sequence.concat_map (Ic_monad.eval ~prev_pins t) ~f:(function pins, res ->
      (let state = Map.map pins ~f:Pin.of_action in
       match res with
       | First () -> Sequence.of_list [ [ state ] ]
       | Second t ->
           Sequence.map (traces' ~prev_pins:pins t) ~f:(fun trace ->
               state :: trace)))

let traces t =
  traces' ~prev_pins:Gpio_hat.Pin.Map.empty t
  |> Sequence.map ~f:(List.drop_while ~f:Map.is_empty)
