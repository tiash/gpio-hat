open Core

module Pin : sig
  type t =
    | Not_connected
    | Constant of bool
    | Input of bool
    | Output of bool
    | Mismatch of bool
  [@@deriving sexp_of, compare]

  val to_string_ansi : ?prev:t -> t -> string
end

module State : sig
  type t = Pin.t Gpio_hat.Pin.Map.t [@@deriving sexp_of, compare]

  val to_string_ansi : ?prev:t -> t -> string
end

type t = State.t list [@@deriving sexp_of, compare]

val to_string_ansi : t -> string

val traces : unit Ic_monad.t -> t Sequence.t
