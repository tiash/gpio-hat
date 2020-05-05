open Core
open Ic_tester
open Logic.Let_syntax

let uint_of_bits bits =
  List.rev bits
  |> List.foldi ~init:0 ~f:(fun i sum bit ->
         if bit then sum lor (1 lsl i) else sum)

let uint_to_bits ~n value =
  List.init n ~f:(fun i -> (value land (1 lsl i)) <> 0) |> List.rev

let uint bits =
  let%map bits = Logic.all bits in
  uint_of_bits bits

let uint' bits value =
  Logic.all_unit
    (List.mapi (List.rev bits) ~f:(fun i bit -> bit (value land (1 lsl i) <> 0)))

let not' bit value = bit (not value)
