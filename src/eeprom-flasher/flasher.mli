open! Core

module Make (T : sig
val name : string
  val config_pins :
    cs:bool -> oe:bool -> we:bool -> (Gpio_hat.Pin.t * bool) list

  val addr : Gpio_hat.Pin.t list

  val data : Gpio_hat.Pin.t list

  val read_latency : Time.Span.t

  val write_pulse : Time.Span.t

  val write_timeout : Time.Span.t

  val write_finished : expected:int -> actual:int -> bool
end) : sig
  module Addr : sig
    val bits : int

    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t

    val of_int : int -> t

    val to_int : t -> int
  end

  module Data : sig
    val bits : int

    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t

    val of_int : int -> t

    val to_int : t -> int
  end

  val capacity : int

  val read_word : Addr.t -> Data.t

  val write_word : Addr.t -> Data.t -> unit

  val read : ?pos:Addr.t -> ?length:int -> unit -> Data.t list

  val write : ?pos:Addr.t -> Data.t list -> unit

  val command : Command.t
end
