open Core

module Make (Word : sig
  val bits : int

  type t

  val of_int : int -> t
end) : sig
  val param : (int -> Word.t list) Command.Param.t
end
