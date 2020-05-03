open Core
open Ic_tester
open Seventy_four_series

let model =
  Model.create "74245" ~summary:"Octal Bust Tranciever with 3-State Outputs"
    ~description:""
    (let%map_open.Dip20 dir = input "DIR" 1
     and a =
       all
         (List.init 8 ~f:(fun n ->
              input_output (sprintf "A_%d" (n + 1)) (n + 2)))
     and b =
       all
         (List.init 8 ~f:(fun n ->
              input_output (sprintf "B_%d" (n + 1)) (18 - n)))
     and enabled = input "~E" 19 >>|* not in
     let%bind dir = dir and enabled = enabled in
     List.map2_exn a b ~f:(fun a b ->
         if not enabled then Logic.all_unit [ snd a false; snd b false ]
         else if dir then
           let%bind v = fst a in
           snd b v
         else
           let%bind v = fst b in
           snd a v)
     |> Logic.all_unit)
