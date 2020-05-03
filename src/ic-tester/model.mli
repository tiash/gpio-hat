open! Core

type t

val create :
  string ->
  ?aliases:string list ->
  summary:string ->
  description:string ->
  unit Logic.t Pins.t ->
  t

val name : t -> string

val summary : t -> string

val to_string : t -> string

val aliases : t -> string list

val description : t -> string

val logic : t -> unit Logic.t

val to_string_pinout : t -> string

val truth_table : t -> string list -> string
