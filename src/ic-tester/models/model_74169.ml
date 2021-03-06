open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74169" ~summary:"Synchronous 4-bit Up/Down Binary Counter"
    ~description:""
    (let%map_open.Dip16 n =
       all [ input "N_4" 6; input "N_3" 5; input "N_2" 4; input "N_1" 3 ]
       >>| Util.uint
     and q =
       all
         [ output "Q_4" 11; output "Q_3" 12; output "Q_2" 13; output "Q_1" 14 ]
       >>| Util.uint'
     and dir = input "U/~D" 1
     and clock = input' "CLK" 2
     and enp = input "~ENP" 7 >>|* not
     and ent = input "~ENT" 10 >>|* not
     and load = input' "~LOAD" 9 >>| Util.not'
     and carry = output "~CARRY" 15 >>| Util.not' in
     let%bind () = Logic.all_unit [ load false; clock false ] in
     let%bind () = sync in
     let check ~ent ~dir n =
       let%map () = q n and () = carry (ent && (if dir then n=0xf else n = 0x0)) in
       n
     in
     let load =
       let%bind () = sync in
       let%bind n = n and ent = ent and (_:bool) = enp and dir = dir and () = load true in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let%bind () = clock false and () = load false in
       check ~ent ~dir n
     in
     let step ~n =
       let%bind () = sync in
       let%bind enp = sample enp and ent = sample ent and dir = sample dir in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let%bind () = clock false in
       let%bind () = sync in
       check ~ent ~dir ((if enp && ent then if dir then n + 1 else n - 1 else n) land 0xf)
     in
     let rec count ~n ~steps =
       if steps <= 0 then return ()
       else
         let%bind n = step ~n in
         count ~n ~steps:(steps - 1)
     in
     let%bind n = load in
     count ~n ~steps:10 |> Logic.ignore_m)
