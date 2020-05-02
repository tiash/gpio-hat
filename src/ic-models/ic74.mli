open Core

type t [@@deriving sexp_of]

val name : t -> string

val aliases : t -> string list

val description : t -> string

val test : t -> unit Ic_tester.Monad.t

val all : t list

val of_string : string -> t

val arg_type : t Command.Arg_type.t
