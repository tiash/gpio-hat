open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74138" ~summary:"3-line to 8-line decoder" ~description:""
    (let%map_open.Dip16 n =
       all [ input "A_2" 3; input "A_1" 2; input "A_0" 1 ] >>| Util.uint
     and enabled =
       all [ input "~E_1" 4 >>|* not; input "~E_2" 5 >>|* not; input "E_3" 6 ]
       >>| Logic.all >>|* List.for_all ~f:Fn.id
     and y =
       all
         [
           output "Y_0" 15;
           output "Y_1" 14;
           output "Y_2" 13;
           output "Y_3" 12;
           output "Y_4" 11;
           output "Y_5" 10;
           output "Y_6" 9;
           output "Y_7" 7;
         ]
     in
     let%bind n = n and enabled = enabled in
     Logic.all_unit (List.mapi y ~f:(fun i y -> y (not enabled || n <> i))))
