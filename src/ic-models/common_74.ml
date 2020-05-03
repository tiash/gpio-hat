open! Core

open Ic_tester
open Ic_tester.Monad.Let_syntax

let pin = Gpio_hat.Pin.of_string

let dip14 name ~summary ?aliases ~description spec =
  Model.create name ~summary ?aliases ~description 
    (let%map_open.Model spec = spec
and () = vcc (pin "B1")
and () = gnd (pin "A7")
in
spec)
let dip16 name ~summary ?aliases ~description spec =
  Model.create name ~summary ?aliases ~description 
    (let%map_open.Model spec = spec
and () = vcc (pin "B1")
and () = gnd (pin "A8")
in
spec)
let dip20 name ~summary ?aliases ~description spec =
  Model.create name ~summary ?aliases ~description 
    (let%map_open.Model spec = spec
and () = vcc (pin "B1")
and () = gnd (pin "A10")
in
spec)

let gate_1_input n a b =
  let%map_open.Model
    a =  input (sprintf "IN%d" n) (pin a)
and b = output (sprintf "OUT%d" n) (pin b)
in
a,b

let hex_1_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description 
    (let%map_open.Model 
gate1 = gate_1_input 1 "A1" "A2" and
gate2 = gate_1_input 2 "A3" "A4" and
gate3 = gate_1_input 3 "A5" "A6" and
gate4 = gate_1_input 4 "B2" "A3" and
gate5 = gate_1_input 5 "B4" "A5" and
gate6 = gate_1_input 6 "B6" "A7" in
let f (in_,out_) = let%bind in_ = in_ in out_ (f in_) in
Monad.all_unit 
[ f gate1; f gate2; f gate3
; f gate4; f gate5; f gate6 
])

let gate_2_input n a b z =
  let%map_open.Model
    a = input (sprintf "A%d" n) (pin a)
    and b = input (sprintf "B%d" n) (pin b)
and z = output (sprintf "Z%d" n) (pin z)
in
a,b,z

let quad_2_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description 
    (let%map_open.Model 
gate1 = gate_2_input 1 "A1" "A2" "A3" and
gate2 = gate_2_input 2 "A4" "A5" "A6" and
gate3 = gate_2_input 3 "B2" "B3" "B4" and 
gate4 = gate_2_input 4 "B5" "B6" "B7" in
let f (a,b,z) = let%bind a = a and b = b in z (f a b) in
Monad.all_unit 
[ f gate1; f gate2; f gate3; f gate4 ])
let quad_2_input_alt name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description 
    (let%map_open.Model 
gate1 = gate_2_input 1 "A2" "A3" "A1" and
gate2 = gate_2_input 2 "A5" "A6" "A4" and
gate3 = gate_2_input 3 "B3" "B4" "B2" and
gate4 = gate_2_input 4 "B6" "B7" "B5" in
let f (a,b,z) = let%bind a = a and b = b in z (f a b) in
Monad.all_unit 
[ f gate1; f gate2; f gate3; f gate4 ])

let gate_3_input n a b c z =
  let%map_open.Model
    a =  input (sprintf "A%d" n) (pin a)
    and b =  input (sprintf "B%d" n) (pin b)
    and c =  input (sprintf "C%d" n) (pin c)
and z = output (sprintf "Z%d" n) (pin z)
in
a,b,c,z

let triple_3_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description 
    (let%map_open.Model 
gate1 = gate_3_input 1 "A1" "A2" "B2" "B3" and
gate2 = gate_3_input 2 "A3" "A4" "A5" "A6" and
gate3 = gate_3_input 3 "B4" "B5" "B6" "B7" in
let f (a,b,c,z) = let%bind a = a and b = b and c = c in z (f a b c) in
Monad.all_unit 
[ f gate1; f gate2; f gate3])

let gate_4_input n a b c d z =
  let%map_open.Model
    a =  input (sprintf "A%d" n) (pin a)
    and b =  input (sprintf "B%d" n) (pin b)
    and c =  input (sprintf "C%d" n) (pin c)
    and d =  input (sprintf "D%d" n) (pin d)
and z = output (sprintf "Z%d" n) (pin z)
in
a,b,c,d,z

let dual_4_input name ~summary ?aliases ~description f =
  dip14 name ~summary ?aliases ~description 
    (let%map_open.Model 
gate1 = gate_4_input 1 "A1" "A2" "A3" "A5" "A6" and
gate2 = gate_4_input 2 "B2" "B3" "B5" "B6" "B7" and
() = nc (pin "A4") and () = nc (pin "B4")
in
let f (a,b,c,d,z) = let%bind a = a and b = b and c = c and d = d in z (f a b c d) in
Monad.all_unit 
[ f gate1; f gate2])

