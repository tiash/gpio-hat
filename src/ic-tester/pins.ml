open Core
module Expert = struct
module Kind = struct
  type t = Input | Output | Input_output | Constant of bool | Not_connected
  [@@deriving sexp_of, equal]
end

module Spec = struct
  type t = { name : string; kind : Kind.t; pin : Gpio_hat.Pin.t }
  [@@deriving sexp_of, equal, fields]
end

type 'a t = { pins : Spec.t Gpio_hat.Pin.Map.t; value : 'a }
[@@deriving sexp_of,fields]

let pins t = Map.data t.pins

let make_pin = function
  | `L n -> Gpio_hat.Pin.of_string (sprintf "A%d" n)
  | `R n -> Gpio_hat.Pin.of_string (sprintf "B%d" n)

let create name pin kind value =
  { pins = Gpio_hat.Pin.Map.singleton pin { Spec.name; kind; pin }; value }

  let return value = { pins = Gpio_hat.Pin.Map.empty; value }

  let map { pins; value } ~f = { pins; value = f value }

  let both a b =
    {
      pins =
        Map.merge_skewed a.pins b.pins ~combine:(fun ~key:_ a b ->
            if Spec.equal a b then a
            else
              raise_s
                [%message
                  "Conflicting pin specifications" (a : Spec.t) (b : Spec.t)]);
      value = (a.value, b.value);
    }

  let not_connected pin = create "NC" (make_pin pin) Not_connected ()

  let constant name pin value = create name (make_pin pin) (Constant value) ()

  let vcc pin = constant "VCC" pin true

  let gnd pin = constant "GND" pin false

  let input name pin =
    let pin = make_pin pin in
    create name pin Input (Logic.Expert.input pin)

  let input' name pin =
    let pin = make_pin pin in
    create name pin Input (Logic.Expert.input' pin)

  let output name pin =
    let pin = make_pin pin in
    create name pin Output (Logic.Expert.output pin)

  let input_output name pin =
    let pin = make_pin pin in
    create name pin Input_output
      (Logic.Expert.input pin, Logic.Expert.output pin)

  let input_output' name pin =
    let pin = make_pin pin in
    create name pin Input_output
      (Logic.Expert.input' pin, Logic.Expert.output pin)
end

include Expert

module Let_syntax = struct
  module Let_syntax = struct
    include Expert
    module Open_on_rhs = Expert
  end
end

