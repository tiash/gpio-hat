open! Core

type 'a t

module type S = Pins_intf.S with type 'a t:='a t

module Make(Pin : sig
type t
val to_string : t-> string
val pin : t -> Gpio_hat.Pin.t
val fixed_pins : (t * string * [`High|`Low|`Not_connected]) list
end) : S with type pin:=Pin.t

include S with type pin := Gpio_hat.Pin.t

val combine : unit Logic.t t list -> unit Logic.t t

module Expert : sig
  module Kind : sig
    type t = Input | Output | Input_output | Constant of bool | Not_connected
    [@@deriving sexp_of]
  end

  module Spec : sig
    type t [@@deriving sexp_of]

    val name : t -> string

    val pin : t -> Gpio_hat.Pin.t

    val kind : t -> Kind.t
  end

  val pins : _ t -> Spec.t list

  val value : 'a t -> 'a
end

