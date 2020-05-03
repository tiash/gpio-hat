open! Core

type 'a t

module Let_syntax : sig
  module Let_syntax : sig
    val map : 'a t -> f:('a -> 'b) -> 'b t

    val both : 'a t -> 'b t -> ('a * 'b) t

    val return : 'a -> 'a t

    module Open_on_rhs : sig
      val vcc : [ `L of int | `R of int ] -> unit t

      val gnd : [ `L of int | `R of int ] -> unit t

      val not_connected : [ `L of int | `R of int ] -> unit t

      val constant : string -> [ `L of int | `R of int ] -> bool -> unit t

      val input : string -> [ `L of int | `R of int ] -> bool Logic.t t

      val input' :
        string -> [ `L of int | `R of int ] -> (bool -> unit Logic.t) t

      val output :
        string -> [ `L of int | `R of int ] -> (bool -> unit Logic.t) t

      val input_output :
        string ->
        [ `L of int | `R of int ] ->
        (bool Logic.t * (bool -> unit Logic.t)) t

      val input_output' :
        string ->
        [ `L of int | `R of int ] ->
        ((bool -> unit Logic.t) * (bool -> unit Logic.t)) t
    end
  end
end

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
