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
  Model.create "7402" ~aliases:[ "74LS02" ] ~summary:"Quad 2-Input NOR Gate"
    ~description:""
    (Pins.combine
       [
         gate "1" ~a:2 ~b:3 ~y:1;
         gate "2" ~a:5 ~b:6 ~y:4;
         gate "3" ~a:8 ~b:9 ~y:10;
         gate "4" ~a:11 ~b:12 ~y:13;
       ])
