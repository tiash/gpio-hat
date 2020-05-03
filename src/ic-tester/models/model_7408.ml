open Core
open Ic_tester
open Seventy_four_series

let gate n a b y =
  let%map_open.Dip14 a = input (sprintf "A_%s" n) a
  and b = input (sprintf "B_%s" n) b
  and y = output (sprintf "Y_%s" n) y in
  let%bind a = a and b = b in
  y ((a && b))

let model =
  Model.create
    "7408" ~aliases:[ "74LS08"]
    ~summary:"Quad 2-Input AND Gate" ~description:"" 
    (Pins.combine
      [ gate "1" 1 2 3
      ; gate "2" 4 5 6
      ; gate "3" 9 10 8
      ; gate "4" 12 13 11
      ])
