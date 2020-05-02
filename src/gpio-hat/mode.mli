open! Core

type t = Input | Output | Not_connected [@@deriving sexp_of]
