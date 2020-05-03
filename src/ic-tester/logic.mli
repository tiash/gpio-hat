open Core

module Action : sig
  type t = Input of bool | Output of bool | Constant of bool | Not_connected
  [@@deriving sexp_of, compare]

  val compatible : t -> t -> bool
end

type 'a t

module Common : sig
  val sync : unit t

  val choice : 'a t list -> 'a t
end

module Let_syntax : sig
  include module type of Common

  include Monad.Infix with type 'a t := 'a t

  val return : 'a -> 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t

    val bind : 'a t -> f:('a -> 'b t) -> 'b t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end

include Monad.S with type 'a t := 'a t and module Let_syntax := Let_syntax

include module type of Common

(* Direct manipulation of pins, should use the let syntax in Model. *)
module Expert : sig
  val input : Gpio_hat.Pin.t -> bool t

  val input' : Gpio_hat.Pin.t -> bool -> unit t

  val output : Gpio_hat.Pin.t -> bool -> unit t

  val constant : Gpio_hat.Pin.t -> bool -> unit t

  val not_connected : Gpio_hat.Pin.t -> unit t
end

val eval :
  ?prev_pins:Action.t Gpio_hat.Pin.Map.t ->
  'a t ->
  (Action.t Gpio_hat.Pin.Map.t * ('a, 'a t) Either.t) Sequence.t
