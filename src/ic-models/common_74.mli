open! Core

val dip14 : string -> summary:string -> ?aliases:string list -> description:string -> unit Ic_tester.Monad.t Ic_tester.Model.Builder.t -> Ic_tester.Model.t
val dip16 : string -> summary:string -> ?aliases:string list -> description:string -> unit Ic_tester.Monad.t Ic_tester.Model.Builder.t -> Ic_tester.Model.t
val dip20 : string -> summary:string -> ?aliases:string list -> description:string -> unit Ic_tester.Monad.t Ic_tester.Model.Builder.t -> Ic_tester.Model.t

val hex_1_input : string -> summary:string -> ?aliases:string list -> description:string -> (bool -> bool) -> Ic_tester.Model.t
val quad_2_input : string -> summary:string -> ?aliases:string list -> description:string -> (bool -> bool -> bool) -> Ic_tester.Model.t
val quad_2_input_alt : string -> summary:string -> ?aliases:string list -> description:string -> (bool -> bool -> bool) -> Ic_tester.Model.t
val triple_3_input : string -> summary:string -> ?aliases:string list -> description:string -> (bool -> bool -> bool-> bool) -> Ic_tester.Model.t
val dual_4_input : string -> summary:string -> ?aliases:string list -> description:string -> (bool -> bool -> bool -> bool -> bool) -> Ic_tester.Model.t
