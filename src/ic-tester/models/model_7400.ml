open Core
open Ic_tester
open Seventy_four_series

let gate n ~a ~b ~y =
  let%map_open.Dip14 a = input (sprintf "A_%s" n) a
  and b = input (sprintf "B_%s" n) b
  and y = output (sprintf "Y_%s" n) y in
  let%bind a = a and b = b in
  y (not (a && b))

let model =
  Model.create "7400" ~summary:"Quad 2-Input NAND Gate" ~description:""
    (Pins.combine
       [
         gate "1" ~a:1 ~b:2 ~y:3;
         gate "2" ~a:4 ~b:5 ~y:6;
         gate "3" ~a:9 ~b:10 ~y:8;
         gate "4" ~a:12 ~b:13 ~y:11;
       ])
