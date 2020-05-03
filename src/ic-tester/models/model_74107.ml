open Core
open Ic_tester
open Seventy_four_series

let gate n ~j ~k ~q ~not_q ~clk ~not_clr =
  let%map_open.Dip14 j = input (sprintf "J_%s" n) j
  and k = input (sprintf "K_%s" n) k
  and q = output (sprintf "Q_%s" n) q
  and not_q = output (sprintf "~Q_%s" n) not_q
  and clr = input' (sprintf "~CLR_%s" n) not_clr >>| fun f v -> f (not v)
  and clk = input' (sprintf "CLK_%s" n) clk in
  let check state =
    let%map () = Logic.all_unit [ q state; not_q (not state) ] in
    state
  in
  let clear =
    let%bind () = clr true in
    let%bind () = sync in
    let%bind () = clr false in
    check false
  in
  let tick state =
    let%bind j = j and k = k in
    let%bind () = clk true in
    let%bind () = sync in
    let%bind () = clk false in
    let%bind state =
      check
        ( match (j, k) with
        | false, false -> state
        | true, false -> true
        | false, true -> false
        | true, true -> not state )
    in
    let%bind () = sync in
    let%bind () = clk true in
    check state
  in
  Logic.ignore_m (clear >>= tick >>= tick)

let model =
  Model.create "74107" ~aliases:[ "74LS107" ] ~summary:"Dual J-K Flip Flop"
    ~description:""
    (Pins.combine
       [
         gate "1" ~j:1 ~k:4 ~q:3 ~not_q:2 ~not_clr:13 ~clk:12;
         gate "2" ~j:8 ~k:11 ~q:5 ~not_q:6 ~not_clr:10 ~clk:9;
       ])
