open Core
open Ic_tester
open Seventy_four_series

let gate n ~a ~b ~z =
  let%map_open.Dip16 a = input (sprintf "A_%s" n) a
  and b = input (sprintf "B_%s" n) b
  and z = output (sprintf "B_%s" n) z in
  fun ~enabled ~s ->
    let%bind a = a and b = b in
    z (enabled && if s then a else b)

let model =
  Model.create "74157" ~aliases:[ "74LS157" ]
    ~summary:"Quad 2-Input Multiplexer" ~description:""
    (let%map_open.Dip16 s = input "S" 1
     and enabled = input "~E" 15 >>|* not
     and gates =
       all
         [
           gate "1" ~a:2 ~b:3 ~z:4;
           gate "2" ~a:5 ~b:6 ~z:7;
           gate "3" ~a:14 ~b:13 ~z:12;
           gate "4" ~a:11 ~b:10 ~z:9;
         ]
     in
     let%bind enabled = enabled and s = s in
     Logic.all_unit (List.map gates ~f:(fun gate -> gate ~enabled ~s)))
