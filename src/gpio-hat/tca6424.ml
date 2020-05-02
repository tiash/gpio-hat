open Core

module I2c = struct
  let to_u8 = Smbus.Uint8.of_int_exn

  let of_u8 = Smbus.Uint8.to_int

  let get =
    Memo.general (fun bus ->
        let last_addr = ref (-1) in
        let i2c = Smbus.Bus.create bus in
        fun addr ->
          if not (equal addr !last_addr) then
            Smbus.Bus.set_address i2c (to_u8 addr) ~force:true;
          i2c)

  let rec read t ~pos ~bits =
    if bits = 0 then 0
    else
      let low =
        if bits land 0xff <> 0 then
          (Smbus.Bus.read_byte_data t (to_u8 pos) |> of_u8) land bits
        else 0
      in
      assert (low land 0xff = low);
      let high = read t ~pos:(pos + 1) ~bits:(bits lsr 8) lsl 8 in
      high lor low land bits

  let rec write t ~pos ~bits ?prev value =
    assert (value land bits = value);
    if bits = 0 then ()
    else (
      if bits land 0xff <> 0 then (
        let prev =
          match prev with
          | Some prev -> prev land 0xff
          | None -> Smbus.Bus.read_byte_data t (to_u8 pos) |> of_u8
        in
        let value = prev land lnot bits lor (value land bits) land 0xff in
        if value <> prev then
          Smbus.Bus.write_byte_data t (to_u8 pos) (to_u8 value);
        () );
      write t ~pos:(pos + 1) ~bits:(bits lsr 8)
        ?prev:(Option.map prev ~f:(fun prev -> prev lsr 8))
        (value lsr 8) )
end

type t = {
  bus : int;
  addr : int;
  mutable input : int;
  mutable output : int;
  mutable output_prev : int;
  mutable mode : int;
  mutable mode_prev : int;
  mutable connected : int;
}

let i2c t = I2c.get t.bus t.addr

let all_pins = 0xffffff

let pin n =
  assert (1 <= n && n <= 24);
  1 lsl (n - 1)

let lnot n = lnot n land all_pins

let check t =
  assert (t.mode land all_pins = t.mode);
  assert (t.output land all_pins = t.output);
  assert (t.connected land all_pins = t.connected);
  assert (t.mode land lnot t.connected = lnot t.connected);
  assert (t.output land lnot t.connected = lnot t.connected);
  assert (t.output land t.mode = t.mode);
  ()

let flush1 t =
  check t;
  I2c.write (i2c t) ~pos:12 ~bits:all_pins ~prev:t.mode_prev
    (t.mode_prev lor t.mode);
  t.mode_prev <- t.mode_prev lor t.mode;
  ()

let flush2 t =
  check t;
  assert (t.mode_prev lor t.mode = t.mode_prev);
  I2c.write (i2c t) ~pos:4 ~bits:all_pins ~prev:t.output_prev t.output;
  t.output_prev <- t.output;
  ()

let flush3 t =
  check t;
  assert (t.output = t.output_prev);
  I2c.write (i2c t) ~pos:12 ~bits:all_pins ~prev:t.mode_prev
    (t.mode_prev land t.mode);
  t.mode_prev <- t.mode_prev land t.mode;
  assert (t.mode_prev = t.mode)

let flush4 t =
  check t;
  assert (t.mode_prev = t.mode);
  t.input <- I2c.read (i2c t) ~pos:0 ~bits:(t.mode land t.connected)

let flush t =
  flush1 t;
  flush2 t;
  flush3 t;
  flush4 t

let set_mode t n (mode : Mode.t) =
  let pin = pin n in
  match mode with
  | Input ->
      t.output <- t.output lor pin;
      t.mode <- t.mode lor pin;
      t.connected <- t.connected lor pin
  | Output ->
      t.input <- t.input land lnot pin;
      t.output <- t.output land lnot pin;
      t.mode <- t.mode land lnot pin;
      t.connected <- t.connected lor pin
  | Not_connected ->
      t.input <- t.input land lnot pin;
      t.output <- t.output lor pin;
      t.mode <- t.mode lor pin;
      t.connected <- t.connected land lnot pin

let mode t n =
  let pin = pin n in
  if t.connected land pin = 0 then Mode.Not_connected
  else if t.mode land pin = 0 then Output
  else Input

let get t n =
  set_mode t n Input;
  let pin = pin n in
  assert (t.mode_prev land pin = pin);
  t.input land pin <> 0

let set t n value =
  set_mode t n Output;
  let pin = pin n in
  assert (t.mode land pin = 0);
  if value then t.output <- t.output lor pin
  else t.output <- t.output land lnot pin

let clear t =
  t.input <- 0;
  t.output <- 0xffffff;
  t.output_prev <- 0;
  t.mode <- 0xffffff;
  t.mode_prev <- 0;
  t.connected <- 0

let reset t =
  clear t;
  flush t

let create ?(bus = 1) ~addr () =
  let t =
    {
      bus;
      addr;
      input = 0;
      output = 0;
      output_prev = 0;
      mode = 0;
      mode_prev = 0;
      connected = 0;
    }
  in
  reset t;
  t
