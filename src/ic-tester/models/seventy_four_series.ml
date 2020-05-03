open! Core
open Ic_tester

module Make (N : sig
  val n : int
end) =
Pins.Make (struct
  open N

  let () = assert (n % 2 = 0)

  type t = int

  let to_string = Int.to_string

  let pin t =
    if 1 <= t && t <= n / 2 then Gpio_hat.Pin.of_string (sprintf "A%d" t)
    else if (n / 2) + 1 <= t && t <= n then
      Gpio_hat.Pin.of_string (sprintf "B%d" (n - t + 1))
    else raise_s [%message "Pin out of range" (t : int) ~min:1 ~max:(n : int)]

  let fixed_pins = [ (n, "VCC", `High); (n / 2, "GND", `Low) ]
end)

module Dip14 = Make (struct
  let n = 14
end)

module Dip16 = Make (struct
  let n = 16
end)

module Dip20 = Make (struct
  let n = 20
end)

(*
let gate_1_input n a b =
  let%map_open.Pins a = input (sprintf "IN%d" n) (a)
  and b = output (sprintf "OUT%d" n) (b) in
  (a, b)

let hex_1_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description
    (let%map_open.Pins gate1 = gate_1_input 1 (`L 1) (`L 2)
     and gate2 = gate_1_input 2 (`L 3) (`L 4)
     and gate3 = gate_1_input 3 (`L 5) (`L 6)
     and gate4 = gate_1_input 4 (`R 2) (`L 3)
     and gate5 = gate_1_input 5 (`R 4) (`L 5)
     and gate6 = gate_1_input 6 (`R 6) (`L 7) in
     let f (in_, out_) =
       let%bind in_ = in_ in
       out_ (f in_)
     in
     Logic.all_unit [ f gate1; f gate2; f gate3; f gate4; f gate5; f gate6 ])

let gate_2_input n a b z =
  let%map_open.Pins a= input (sprintf "A%d" n) (a)
  and b = input (sprintf "B%d" n) (b)
  and z = output (sprintf "Z%d" n) (z) in
  (a, b, z)

let quad_2_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description
    (let%map_open.Pins gate1  = gate_2_input 1 (`L 1) (`L 2) (`L 3)
     and gate2 = gate_2_input 2 (`L 4) (`L 5) (`L 6)
     and gate3 = gate_2_input 3 (`R 2) (`R 3) (`R 4)
     and gate4 = gate_2_input 4 (`R 5) (`R 6) (`R 7) in
     let f (a, b, z) =
       let%bind a = a and b = b in
       z (f a b)
     in
     Logic.all_unit [ f gate1; f gate2; f gate3; f gate4 ])

let quad_2_input_alt name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description
    (let%map_open.Pins gate1 = gate_2_input 1 (`L 2) (`L 3) (`L 1)
     and gate2 = gate_2_input 2 (`L 5) (`L 6) (`L 4)
     and gate3 = gate_2_input 3 (`R 3) (`R 4) (`R 2)
     and gate4 = gate_2_input 4 (`R 6) (`R 7) (`R 5) in
     let f (a, b, z) =
       let%bind a = a and b = b in
       z (f a b)
     in
     Logic.all_unit [ f gate1; f gate2; f gate3; f gate4 ])

let gate_3_input n a b c z =
  let%map_open.Pins a = input (sprintf "A%d" n) (a)
  and b = input (sprintf "B%d" n) (b)
  and c = input (sprintf "C%d" n) (c)
  and z = output (sprintf "Z%d" n) (z) in
  (a, b, c, z)

let triple_3_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description
    (let%map_open.Pins gate1 = gate_3_input 1 (`L 1) (`L 2) (`R 2) (`R 3)
     and gate2 = gate_3_input 2 (`L 3) (`L 4) (`L 5) (`L 6)
     and gate3 = gate_3_input 3 (`R 4) (`R 5) (`R 6) (`R 7) in
     let f (a, b, c, z) =
       let%bind a = a and b = b and c = c in
       z (f a b c)
     in
     Logic.all_unit [ f gate1; f gate2; f gate3 ])

let gate_4_input n a b c d z =
  let%map_open.Pins a = input (sprintf "A%d" n) (a)
  and b = input (sprintf "B%d" n) (b)
  and c = input (sprintf "C%d" n) (c)
  and d = input (sprintf "D%d" n) (d)
  and z = output (sprintf "Z%d" n) (z) in
  (a, b, c, d, z)

let dual_4_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description
    (let%map_open.Pins gate1 = gate_4_input 1 (`L 1) (`L 2) (`L 3) (`L 5) (`L 6)
     and gate2 = gate_4_input 2 (`R 2) (`R 3) (`R 5) (`R 6) (`R 7)
     and () = not_connected (`L 4)
     and () = not_connected (`R 4) in
     let f (a, b, c, d, z) =
       let%bind a = a and b = b and c = c and d = d in
       z (f a b c d)
     in
     Logic.all_unit [ f gate1; f gate2 ])
*)
