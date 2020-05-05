open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74173" ~summary:"4-Bit D-Type Register with 3-State Outputs"
    ~description:""
    (let%map_open.Dip16 () =
(* CR-someday mhorn: hardware can't detect High impedence pins *)
    constant "M" 1 false
and () = constant "N" 2 false
     and q =
       all [ output "Q_4" 6; output "Q_3" 5; output "Q_2" 4; output "Q_1" 3 ]
       >>| Util.uint'
     and d =
       all [ input "D_4" 11; input "D_3" 12; input "D_2" 13; input "D_1" 14 ]
       >>| Util.uint
     and clock = input' "CLK" 7
     and load =
       all [ input "~G_1" 9 >>|* not; input "~G_2" 10 >>|* not ]
       >>| Logic.all >>|* List.for_all ~f:Fn.id
     and clear = input "CLR" 15 in
     let check = function
| None -> return ()
| Some state -> q state
     in
     let%bind () = clock false in
     let step state =
       let%bind d = if Option.is_some state then sample ~n:2 d else d 
       and clear = clear
       in
       let%bind load = if Option.is_some state || clear then sample load else Logic.require' load ~f:Fn.id in
       let%bind () = sync in
       let state = if clear then Some 0 else state in
       let%bind () = check state in
       let%bind () = sync in
       let%bind () = clock true in
       let%bind () = sync in
       let state = if clear then Some 0 else if load then Some d else state in
       let%bind () = check state in
       let%bind () = sync in
       let%bind () = clock false in
       let%bind () = sync in
       let%bind () = check state in
       let%bind () = sync in
       return state
     in
step None >>= step >>= step >>= step >>| (ignore : int option -> unit))
