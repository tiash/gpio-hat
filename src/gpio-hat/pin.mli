open Core

type t = private A of int | B of int [@@deriving sexp_of, compare, enumerate]

include Comparable.S_plain with type t := t

include Stringable.S with type t := t

val arg_type : t Command.Arg_type.t
