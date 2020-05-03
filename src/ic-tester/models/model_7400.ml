open Core
open Common_74

let model =
  quad_2_input "7400" ~aliases:[ "74LS00"; "7403"; "74132" ]
    ~summary:"Quad 2-Input NAND Gate" ~description:"" (fun a b -> not (a && b))
