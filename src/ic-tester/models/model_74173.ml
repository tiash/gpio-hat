open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74173" ~aliases:[ "74LS173" ]
    ~summary:"4-Bit D-Type Register with 3-State Outputs" ~description:""
    (let%map_open.Dip16 output_enabled =
       all [ input "M" 1; input "N" 2 ] >>| Logic.all >>|* List.for_all ~f:Fn.id
     and q =
       all [ output "Q_4" 6; output "Q_3" 5; output "Q_2" 4; output "Q_1" 3 ]
       >>| Util.uint'
     and d =
       all [ input "D_4" 11; input "D_3" 12; input "D_2" 13; input "D_1" 14 ]
       >>| Util.uint
     and clock = input' "CLK" 7
     and load_enabled =
       all [ input "~G_1" 9; input "~G_2" 10 ]
       >>| Logic.all >>|* List.for_all ~f:not
     and clear = input' "CLR" 15 in
     let check state =
       let%bind output_enabled = output_enabled in
       if output_enabled then q state
       else
         (* CR mhorn: Should check that [q] is NC but the hat doesn't have that ability *)
         q 0
     in
     let check' state =
       let%bind _ = d in
       check state
     in
     let clear =
       let%bind () = sync in
       let%bind () = clear true in
       let state = 0 in
       let%bind () = check' state in
       let%bind () = sync in
       let%bind () = clear false in
       let%bind () = check' state in
       return state
     in
     let mset ?(guard = const true) state =
       let%bind () = sync in
       let%bind load_enabled = Logic.require' load_enabled ~f:guard in
       let%bind d = d in
       let%bind () = sync in
       let%bind () = clock true in
       let state = if load_enabled then d else state in
       let%bind () = check state in
       let%bind () = sync in
       let%bind () = check' state in
       let%bind () = sync in
       let%bind () = clock false in
       let%bind () = check' state in
       return state
     in
     let set = mset ~guard:Fn.id (-1) in
     choice [ clear; set >>= mset ] |> Logic.ignore_m)
