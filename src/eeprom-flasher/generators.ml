open! Core

let all = [
"0", List.init ~f:(const 0)
; "255", List.init ~f:(const 0xff)
; "seven-segment", (fun _ -> 
  let a = 0x01 in
  let b = 0x02 in
  let c = 0x04 in
  let d = 0x08 in
  let e = 0x10 in
  let f = 0x20 in
  let g = 0x40 in
  let dp = 0x80 in
  let digit = function
    | 0 -> a lor b lor c lor d lor e lor f
    | 1 -> b lor c
    | 2 -> a lor b lor g lor e lor d
    | 3 -> a lor b lor g lor c lor d
    | 4 -> f lor g lor b lor c
    | 5 -> a lor f lor g lor c lor d
    | 6 -> a lor f lor g lor c lor d lor e
    | 7 -> a lor b lor c
    | 8 -> a lor b lor c lor d lor e lor f lor g
    | 9 -> g lor f lor a lor b lor c
    | 0xA -> e lor f lor a lor b lor c lor g
    | 0xb -> f lor e lor d lor c lor g
    | 0xC -> a lor f lor e lor d
    | 0xd -> g lor e lor d lor c lor b
    | 0xE -> a lor f lor g lor e lor d
    | 0xF -> a lor f lor g lor e
    | _ -> assert false
  in
  let letter_h_dot = f lor g lor e lor c lor dp in
  List.init (2*4*256) ~f:(fun addr ->
      let is_hex = 0x400 land addr <> 0 in
      let pos = (0x300 land addr) lsr 8 in
      let num = 0xff land addr in
        match (is_hex, pos, num) with
        | true, 2, _ -> letter_h_dot
        | true, 1, n -> digit ((n land 0xf0) lsr 4)
        | true, 0, n -> digit (n land 0x0f)
        | false, 2, n -> digit (n / 100)
        | false, 1, n -> digit (n / 10 % 10)
        | false, 0, n -> digit (n % 10) lor dp
        | _, _, _ -> 0))
; "random",( fun len ->
let () = Random.self_init () in
List.init len ~f:(fun _ -> Random.int 255))
]
