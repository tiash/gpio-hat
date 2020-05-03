open! Core

module Builder : sig
type 'a t 
end

module Let_syntax : sig
module Let_syntax : sig
open Builder
  val map : 'a t -> f:('a -> 'b) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t

module Open_on_rhs : sig
  val vcc : Gpio_hat.Pin.t -> unit t
  val gnd : Gpio_hat.Pin.t -> unit t
  val nc : Gpio_hat.Pin.t -> unit t
  val constant : string -> Gpio_hat.Pin.t -> bool -> unit t
  val input : string -> Gpio_hat.Pin.t -> bool Ic_monad.t t
  val input' : string -> Gpio_hat.Pin.t -> (bool -> unit Ic_monad.t) t
  val output : string -> Gpio_hat.Pin.t -> (bool -> unit Ic_monad.t) t
  val input_output : string -> Gpio_hat.Pin.t -> (bool Ic_monad.t * (bool -> unit Ic_monad.t)) t
  val input_output' : string -> Gpio_hat.Pin.t -> ((bool -> unit Ic_monad.t) * (bool -> unit Ic_monad.t)) t
end 
end
end

type t

val create : string -> ?aliases:string list -> summary:string -> description:string -> (unit Ic_monad.t Builder.t) -> t

val name : t -> string
val summary : t -> string
val to_string : t -> string
val aliases : t -> string list
val description : t -> string

val logic : t -> unit Ic_monad.t

val pins : t -> string Gpio_hat.Pin.Map.t

val to_string_pinout : t -> string

val truth_table : t -> Gpio_hat.Pin.t list -> string

module type S = sig
  val model : t
end
