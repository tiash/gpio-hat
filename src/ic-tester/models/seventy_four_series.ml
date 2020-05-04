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

module Pins = struct end
