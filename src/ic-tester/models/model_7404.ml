open Core
open Ic_tester
open Seventy_four_series

let gate n ~a ~y =
  let%map_open.Dip14 a = input (sprintf "A_%s" n) a
  and y = output (sprintf "Y_%s" n) y in
  let%bind a = a in
  y (not a)

let model =
  Model.create "7404" ~summary:"Hex Inverters" ~description:""
    (Pins.combine
       [
         gate "1" ~a:1 ~y:2;
         gate "2" ~a:3 ~y:4;
         gate "3" ~a:5 ~y:6;
         gate "4" ~a:9 ~y:8;
         gate "5" ~a:11 ~y:10;
         gate "6" ~a:13 ~y:12;
       ])
