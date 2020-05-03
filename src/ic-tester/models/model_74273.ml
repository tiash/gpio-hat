open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74273" ~summary:"Octal D Flip-Flop with Clear" ~description:""
    (let%map_open.Dip20 d =
       all
         [
           input "D_0" 3;
           input "D_1" 4;
           input "D_2" 7;
           input "D_3" 8;
           input "D_4" 13;
           input "D_5" 14;
           input "D_6" 17;
           input "D_7" 18;
         ]
       >>| Util.uint
     and q =
       all
         [
           output "Q_0" 2;
           output "Q_1" 5;
           output "Q_2" 6;
           output "Q_3" 9;
           output "Q_4" 12;
           output "Q_5" 15;
           output "Q_6" 16;
           output "Q_7" 19;
         ]
       >>| Util.uint'
     and reset = input "~R" 1 >>|* not
     and clock = input' "CP" 11 in
     let check n = q n in
     let step n0 =
       let%bind n1 = sample ~n:2 d and reset = reset in
       let n = if reset then 0 else n0 in
       let%bind () = check n in
       let%bind () = sync in
       let%bind () = clock true in
       let n = if reset then 0 else n1 in
       let%bind () = check n in
       let%bind () = sync in
       let%bind () = clock false in
       let%bind () = check n in
       let%bind () = sync in
       let%bind () = check n in
       let%bind () = sync in
       return n
     in
     let%bind () = clock false in
     let%bind () = require reset ~f:Fn.id in
     let%bind (_ : int) = sample ~n:2 d in
     let%bind () = check 0 in
     let%bind () = sync in
     Logic.ignore_m (step 0 >>= step >>= step >>= step))
