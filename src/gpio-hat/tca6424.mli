open! Core

type t

val create : ?bus:int -> addr:int -> unit -> t

val flush : t -> unit

val flush1 : t -> unit

val flush2 : t -> unit

val flush3 : t -> unit

val flush4 : t -> unit

val clear : t -> unit

val reset : t -> unit

val get : t -> int -> bool

val set : t -> int -> bool -> unit

val mode : t -> int -> Mode.t

val set_mode : t -> int -> Mode.t -> unit
