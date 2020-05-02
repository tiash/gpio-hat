open! Core
module Pin = Pin
module Mode = Mode

let a = Tca6424.create ~addr:0x22 ()

let b = Tca6424.create ~addr:0x23 ()

let flush () =
  Tca6424.flush1 a;
  Tca6424.flush1 b;
  Tca6424.flush2 a;
  Tca6424.flush2 b;
  Tca6424.flush3 a;
  Tca6424.flush3 b;
  Tca6424.flush4 a;
  Tca6424.flush4 b

let clear () =
  Tca6424.clear a;
  Tca6424.clear b

let reset () =
  clear ();
  flush ()

let get = function Pin.A n -> Tca6424.get a n | Pin.B n -> Tca6424.get b n

let set = function Pin.A n -> Tca6424.set a n | Pin.B n -> Tca6424.set b n

let mode = function Pin.A n -> Tca6424.mode a n | Pin.B n -> Tca6424.mode b n

let set_mode = function
  | Pin.A n -> Tca6424.set_mode a n
  | Pin.B n -> Tca6424.set_mode b n
