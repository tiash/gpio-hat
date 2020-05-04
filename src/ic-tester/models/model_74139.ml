open Core
open Ic_tester
open Seventy_four_series

let gate n ~g ~n:(a, b) ~y:(y0, y1, y2, y3) =
  let%map_open.Dip16 enabled = input (sprintf "not_G_%s" n) g >>|* not
  and n =
    all [ input (sprintf "B_%s" n) b; input (sprintf "A_%s" n) a ] >>| Util.uint
  and y =
    all
      [
        output (sprintf "Y_%s_0" n) y0;
        output (sprintf "Y_%s_1" n) y1;
        output (sprintf "Y_%s_2" n) y2;
        output (sprintf "Y_%s_3" n) y3;
      ]
  in
  let%bind enabled = enabled and n = n in
  List.mapi y ~f:(fun i y -> y (not (enabled && n = i))) |> Logic.all_unit

let model =
  Model.create "74139" ~summary:"Quad 2-Input NAND Gate" ~description:""
    (Pins.combine
       [
         gate "1" ~g:1 ~n:(2, 3) ~y:(4, 5, 6, 7);
         gate "2" ~g:15 ~n:(14, 13) ~y:(12, 11, 10, 9);
       ])
