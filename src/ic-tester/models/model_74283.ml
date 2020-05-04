open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74283" ~summary:"4-Bit Binary Full Adder with Fast Carry"
    ~description:""
    (let%map_open.Dip16 a =
       all [ input "A_4" 12; input "A_3" 14; input "A_2" 3; input "A_1" 5 ]
       >>| Util.uint
     and b =
       all [ input "B_4" 11; input "B_3" 15; input "B_2" 2; input "B_1" 6 ]
       >>| Util.uint
     and c = all [ input "C_1" 7 ] >>| Util.uint
     and z =
       all
         [
           output "Z_5" 9;
           output "Z_4" 10;
           output "Z_3" 13;
           output "Z_2" 1;
           output "Z_1" 4;
         ]
       >>| Util.uint'
     in
     let%bind a = a and b = b and c = c in
     z (a + b + c))
