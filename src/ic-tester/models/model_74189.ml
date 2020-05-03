open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74189" ~aliases:[ "74LS189" ]
    ~summary:"64-bit random access memory" ~description:""
    (let%map_open.Dip16 addr =
       all [ input "A_3" 13; input "A_2" 14; input "A_1" 15; input "A_0" 1 ]
       >>| Util.uint
     and data =
       all [ input "D_3" 12; input "D_2" 10; input "D_1" 6; input "D_0" 4 ]
       >>| Util.uint
     and output =
       all
         [
           output "~O_3" 11 >>| Util.not';
           output "~O_2" 9 >>| Util.not';
           output "~O_1" 7 >>| Util.not';
           output "~O_0" 5 >>| Util.not';
         ]
       >>| Util.uint'
     and cs = input "~CS" 2 >>|* not
     and we = input "~WE" 3 >>|* not in
     let rec walk ~to_write ~to_read ~state =
       if Map.is_empty to_write && Set.is_empty to_read then return ()
       else
         let%bind () = sync in
         let%bind addr = sample addr
         and data = sample data
         and cs = sample cs in
         if not cs then
           let%bind (_ : bool) = sample we and () = output 0 in
           walk ~to_write ~to_read ~state
         else
           let%bind to_read =
             match Map.find state addr with
             | None -> return to_read
             | Some value ->
                 let%map () = output value in
                 Set.remove to_read addr
           in
           let%bind write = sample we in
           if write then
             let to_write =
               Map.change to_write addr ~f:(function
                 | None -> None
                 | Some to_write ->
                     let to_write = Set.remove to_write data in
                     if Set.is_empty to_write then None else Some to_write)
             in
             let state = Map.set state ~key:addr ~data in
             let%bind () = sync in
             let%bind () = output data in
             let%bind () = Logic.require we ~f:not in
             let%bind () = sync in
             let%bind () = output data in
             let%bind () = sync in
             walk ~to_write ~to_read ~state
           else walk ~to_write ~to_read ~state
     in
     let to_write =
       List.init 16 ~f:(fun n -> (n, List.init 16 ~f:Fn.id |> Int.Set.of_list))
       |> Int.Map.of_alist_exn
     in
     let to_read = List.init 16 ~f:Fn.id |> Int.Set.of_list in
     let state = Int.Map.empty in
     walk ~to_write ~to_read ~state)
