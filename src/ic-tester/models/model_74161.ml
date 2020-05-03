open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74161" ~aliases:[ "74LS161" ]
    ~summary:"Synchronous 4-bit counter" ~description:""
    (let%map_open.Dip16 n =
       all [ input "N_4" 6; input "N_3" 5; input "N_2" 4; input "N_1" 3 ]
       >>| Util.uint
     and q =
       all
         [ output "Q_4" 11; output "Q_3" 12; output "Q_2" 13; output "Q_1" 14 ]
       >>| Util.uint'
     and clear = input' "~CLR" 1 >>| fun f v -> f (not v)
     and clock = input' "CLK" 2
     and enp = input "ENP" 7
     and ent = input "ENT" 10
     and load = input' "~LOAD" 9 >>| fun f v -> f (not v)
     and carry = output "CARRY" 15 in
     let%bind () = Logic.all_unit [ load false; clear false; clock false ] in
     let%bind () = sync in
     let check n =
       let%map () = q n and () = carry (n = 0xf) in
       n
     in
     let clear =
       let%bind () = sync in
       let%bind () = clear true in
       let%bind () = sync in
       let%bind () = clear true in
       check 0
     in
     let load =
       let%bind n = n and () = load true in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let%bind () = clock false and () = load false in
       check n
     in
     let step ~n =
       let%bind enp = enp and ent = ent in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let%bind () = clock false in
       check (if enp && ent then n + 1 else n)
     in
     let rec count ~n ~steps =
       if steps <= 0 then return ()
       else
         let%bind n = step ~n in
         count ~n ~steps:(steps - 1)
     in
     let%bind n = choice [ clear; load ] in
     count ~n ~steps:5 |> Logic.ignore_m)
