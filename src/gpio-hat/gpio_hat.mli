open !Core
module Pin = Pin
module Mode = Mode

val reset : unit -> unit

val clear : unit -> unit

val flush : unit -> unit

val get : Pin.t -> bool

val set : Pin.t -> bool -> unit

val mode : Pin.t -> Mode.t

val set_mode : Pin.t -> Mode.t -> unit
