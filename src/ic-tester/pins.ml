open Core

module Expert = struct
  module Kind = struct
    type t = Input | Output | Input_output | Constant of bool | Not_connected
    [@@deriving sexp_of, equal]
  end

  module Spec = struct
    type t = { id : string; name : string; kind : Kind.t; pin : Gpio_hat.Pin.t }
    [@@deriving sexp_of, equal, fields]
  end

  type 'a t = { pins : Spec.t Gpio_hat.Pin.Map.t; value : 'a }
  [@@deriving sexp_of, fields]

  let pins t = Map.data t.pins

  let merge_pins a b =
    Map.merge_skewed a b ~combine:(fun ~key:_ a b ->
        if Spec.equal a b then a
        else
          raise_s
            [%message
              "Conflicting pin specifications" (a : Spec.t) (b : Spec.t)])
end

include Expert

module type S = Pins_intf.S with type 'a t := 'a t

module Make (Pin : sig
  type t

  val to_string : t -> string

  val pin : t -> Gpio_hat.Pin.t

  val fixed_pins : (t * string * [ `Not_connected | `High | `Low ]) list
end) =
struct
  module Impl = struct
    include Expert

    let make_pin ~name ~pin ~kind =
      { Spec.id = Pin.to_string pin; name; kind; pin = Pin.pin pin }

    let fixed_pins =
      List.map Pin.fixed_pins ~f:(fun (pin, name, kind) ->
          let kind =
            match kind with
            | `Not_connected -> Kind.Not_connected
            | `High -> Constant true
            | `Low -> Constant false
          in
          let t = make_pin ~name ~pin ~kind in
          (t.pin, t))
      |> Gpio_hat.Pin.Map.of_alist_exn

    let create ~name ~pin ~kind value =
      let pin = make_pin ~name ~pin ~kind in
      { pins = Map.add_exn fixed_pins ~key:pin.pin ~data:pin; value }

    let not_connected pin = create ~name:"NC" ~pin ~kind:Not_connected ()

    let constant name pin value = create ~name ~pin ~kind:(Constant value) ()

    let vcc pin = constant "VCC" pin true

    let gnd pin = constant "GND" pin false

    let input name pin =
      create ~name ~pin ~kind:Input (Logic.Expert.input (Pin.pin pin))

    let input' name pin =
      create ~name ~pin ~kind:Input (Logic.Expert.input' (Pin.pin pin))

    let output name pin =
      create ~name ~pin ~kind:Output (Logic.Expert.output (Pin.pin pin))

    let input_output name pin =
      create ~name ~pin ~kind:Input_output
        (Logic.Expert.input (Pin.pin pin), Logic.Expert.output (Pin.pin pin))

    let input_output' name pin =
      create ~name ~pin ~kind:Input_output
        (Logic.Expert.input' (Pin.pin pin), Logic.Expert.output (Pin.pin pin))

    let return value = { pins = fixed_pins; value }

    let both a b =
      {
        pins = merge_pins (merge_pins a.pins b.pins) fixed_pins;
        value = (a.value, b.value);
      }

    let map t ~f = { pins = merge_pins t.pins fixed_pins; value = f t.value }

    let ( >>| ) t f = map t ~f

    let ( >>=* ) t f = map t ~f:(Logic.bind ~f)

    let ( >>|* ) t f = map t ~f:(Logic.map ~f)

    let rec all = function
      | [] -> return []
      | hd :: tl -> map (both hd (all tl)) ~f:(fun (hd, tl) -> hd :: tl)
  end

  module Let_syntax = struct
    include Impl

    module Let_syntax = struct
      include Impl
      module Open_on_rhs = Impl
    end
  end

  include Impl
end

include Make (struct
  type t = Gpio_hat.Pin.t

  let to_string = Gpio_hat.Pin.to_string

  let pin = Fn.id

  let fixed_pins = []
end)

let rec all = function
  | [] -> return []
  | hd :: tl ->
      let%map hd = hd and tl = all tl in
      hd :: tl

let combine ts =
  let%map ts = all ts in
  Logic.all_unit ts
