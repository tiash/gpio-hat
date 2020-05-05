open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74161" ~summary:"Synchronous 4-bit counter" ~description:""
    (let%map_open.Dip16 n =
       all [ input "N_4" 6; input "N_3" 5; input "N_2" 4; input "N_1" 3 ]
       >>| Util.uint
     and q =
       all
         [ output "Q_4" 11; output "Q_3" 12; output "Q_2" 13; output "Q_1" 14 ]
       >>| Util.uint'
     and clear = input' "~CLR" 1 >>| Util.not'
     and clock = input' "CLK" 2
     and enp = input "ENP" 7
     and ent = input "ENT" 10
     and load = input' "~LOAD" 9 >>| Util.not'
     and carry = output "CARRY" 15 in
     let%bind () = Logic.all_unit [ load false; clear false; clock false ] in
     let%bind () = sync in
     let check ~ent n =
       let%map () = q n and () = carry (ent && (n = 0xf)) in
       n
     in
     let clear =
       let%bind () = sync in
       let%bind ent = ent and (_:bool) = enp and (_:int) = n in
       let%bind () = clear true in
       let%bind () = sync in
       let%bind state = check ~ent 0 in
       let%bind () = clear false in
       let%bind () = sync in
       check ~ent state
     in
     let load =
       let%bind () = sync in
       let%bind ent = ent and (_:bool) = enp and n = n and () = load true in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let%bind n = check ~ent n in
       let%bind () = clock false and () = load false in
       check ~ent n
     in
     let step ~n =
       let%bind () = sync in
       let%bind enp = sample enp and ent = sample ent in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let%bind () = clock false in
       let%bind () = sync in
       let%bind n = check ~ent (if enp && ent then n + 1 else n) in
       let%bind () = sync in
       check ~ent n
     in
     let rec count ~n ~steps =
       if steps <= 0 then return ()
       else
         let%bind n = step ~n in
         count ~n ~steps:(steps - 1)
     in
     let%bind n = choice [ clear; load ] in
     count ~n ~steps:5 |> Logic.ignore_m)
