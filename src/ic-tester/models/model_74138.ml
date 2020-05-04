open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74138" ~summary:"3-line to 8-line decoder" ~description:""
    (let%map_open.Dip16 n =
       all [ input "C" 3; input "B" 2; input "A" 1 ] >>| Util.uint
     and enabled =
       all [ input "G_1" 6; input "~G_2A" 4 >>|* not; input "~G_2B" 5 >>|* not ]
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
     Logic.all_unit (List.mapi y ~f:(fun i y -> y (not (enabled && n = i)))))
