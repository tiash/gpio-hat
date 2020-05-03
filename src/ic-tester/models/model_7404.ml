open Core
open Ic_tester
open Seventy_four_series

let gate n a y =
  let%map_open.Dip14 a = input (sprintf "A_%s" n) a
  and y = output (sprintf "Y_%s" n) y in
  let%bind a = a in
  y (not a)

let model =
  Model.create
    "7404"
    ~summary:"Hex Inverters" ~description:"" 
    (Pins.combine
      [ gate "1" 1 2
      ; gate "2" 3 4 
      ; gate "3" 5 6
      ; gate "4" 9 8
      ; gate "5" 11 10
      ; gate "6" 13 12
      ])
