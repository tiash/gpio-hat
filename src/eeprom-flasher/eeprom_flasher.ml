open! Core

module Eeprom_16C28 = Flasher.Make (struct
let name = "16C28"
  let addr =
    [
      Gpio_hat.Pin.of_string "A6";
      Gpio_hat.Pin.of_string "A3";
      Gpio_hat.Pin.of_string "A2";
      Gpio_hat.Pin.of_string "B1";
      Gpio_hat.Pin.of_string "B2";
      Gpio_hat.Pin.of_string "B3";
      Gpio_hat.Pin.of_string "B4";
      Gpio_hat.Pin.of_string "B5";
      Gpio_hat.Pin.of_string "B6";
      Gpio_hat.Pin.of_string "B7";
      Gpio_hat.Pin.of_string "B8";
    ]

  let data =
    [
      Gpio_hat.Pin.of_string "A8";
      Gpio_hat.Pin.of_string "A9";
      Gpio_hat.Pin.of_string "A10";
      Gpio_hat.Pin.of_string "A11";
      Gpio_hat.Pin.of_string "A12";
      Gpio_hat.Pin.of_string "B11";
      Gpio_hat.Pin.of_string "B10";
      Gpio_hat.Pin.of_string "B9";
    ]

  let config_pins ~cs ~oe ~we =
    [
      (* VCC *)
      (Gpio_hat.Pin.of_string "A1", true);
      (* GND *)
      (Gpio_hat.Pin.of_string "B12", false);
      (* ~CE *)
      (Gpio_hat.Pin.of_string "A7", not cs);
      (* ~OE *)
      (Gpio_hat.Pin.of_string "A5", not oe);
      (* ~WE *)
      (Gpio_hat.Pin.of_string "A4", not we);
    ]

  let read_latency = Time.Span.of_ns 200. (* spec says 150 *)

  let write_pulse = Time.Span.of_ns 200. (* spec says 100 to 1000 *)

  let write_timeout = Time.Span.of_ms 1.1 (* spec says 1 *)

  let write_finished ~expected ~actual = expected land 0x80 = actual land 0x80
end)

let command = Command.group ~summary:"" ["16C28", Eeprom_16C28.command]

(*
let init () =
  Ic.set vss false;
  Ic.set vcc true;
  Ic.set not_ce false;
  Ic.set not_oe true;
  Ic.set not_we true;
  Ic.flush ()

let length () = 0x800

let set_addr addr =
  assert (addr land (length () - 1) = addr);
  Ic.set a0 (addr land 0x001 <> 0);
  Ic.set a1 (addr land 0x002 <> 0);
  Ic.set a2 (addr land 0x004 <> 0);
  Ic.set a3 (addr land 0x008 <> 0);
  Ic.set a4 (addr land 0x010 <> 0);
  Ic.set a5 (addr land 0x020 <> 0);
  Ic.set a6 (addr land 0x040 <> 0);
  Ic.set a7 (addr land 0x080 <> 0);
  Ic.set a8 (addr land 0x100 <> 0);
  Ic.set a9 (addr land 0x200 <> 0);
  Ic.set a10 (addr land 0x400 <> 0)

let set_data data =
  assert (data land 0xff = data);
  Ic.set not_oe true;
  Ic.flush ();
  Ic.set_mode io0 Output;
  Ic.set_mode io1 Output;
  Ic.set_mode io2 Output;
  Ic.set_mode io3 Output;
  Ic.set_mode io4 Output;
  Ic.set_mode io5 Output;
  Ic.set_mode io6 Output;
  Ic.set_mode io7 Output;
  Ic.set io0 (data land 0x01 <> 0);
  Ic.set io1 (data land 0x02 <> 0);
  Ic.set io2 (data land 0x04 <> 0);
  Ic.set io3 (data land 0x08 <> 0);
  Ic.set io4 (data land 0x10 <> 0);
  Ic.set io5 (data land 0x20 <> 0);
  Ic.set io6 (data land 0x40 <> 0);
  Ic.set io7 (data land 0x80 <> 0)

let get_data_prep () =
  Ic.set not_oe false;
  Ic.set_mode io0 Input;
  Ic.set_mode io1 Input;
  Ic.set_mode io2 Input;
  Ic.set_mode io3 Input;
  Ic.set_mode io4 Input;
  Ic.set_mode io5 Input;
  Ic.set_mode io6 Input;
  Ic.set_mode io7 Input

let get_data () =
  (if Ic.get io0 then 0x01 else 0)
  lor (if Ic.get io1 then 0x02 else 0)
  lor (if Ic.get io2 then 0x04 else 0)
  lor (if Ic.get io3 then 0x08 else 0)
  lor (if Ic.get io4 then 0x10 else 0)
  lor (if Ic.get io5 then 0x20 else 0)
  lor (if Ic.get io6 then 0x40 else 0)
  lor if Ic.get io7 then 0x80 else 0

let read_byte ~pos =
  assert (0 <= pos && pos < length ());
  set_addr pos;
  get_data_prep ();
  Ic.flush ();
  Caml.Unix.sleepf 0.0002;
  Ic.flush ();
  let data = get_data () in
  if debug then eprintf "read: %03x: %02x\n%!" pos data;
  data

let write_byte ~pos data =
  assert (0 <= pos && pos < length ());
  assert (data land 0xff = data);
  if debug then eprintf "write: %03x: %02x\n%!" pos data;
  set_addr pos;
  set_data data;
  Ic.flush ();
  Caml.Unix.sleepf 0.0002;
  Ic.flush ();
  Caml.Unix.sleepf 0.0002;
  Ic.set not_we false;
  Ic.flush ();
  Caml.Unix.sleepf 0.0002;
  Ic.set not_we true;
  Ic.flush ();
  Caml.Unix.sleepf 0.0002;
  Ic.flush ();
  let rec wait n =
    Caml.Unix.sleepf 0.0001;
    assert (n > 0);
    if debug then eprintf "wait: %d\n" n;
    get_data_prep ();
    Ic.flush ();
    Caml.Unix.sleepf 0.0001;
    Ic.flush ();
    let data' = get_data () in
    Ic.set not_oe true;
    if data = data' then ()
    else (
      eprintf "DEBUG: %2x <?> %2x\n%!" data data';
      assert (data land 0x80 <> data' land 0x80);
      wait (n - 1) )
  in
  wait 1000;
  let data' = read_byte ~pos in
  assert (data = data')

let read ?(pos = 0) ?(len = length () - pos) () =
  assert (0 <= pos && pos <= length ());
  assert (0 <= len && pos + len <= length ());
  init ();
  Ic.flush ();
  let data =
    String.init len ~f:(fun offs ->
        if (not debug) && offs % 16 = 0 then
          eprintf "reading: %4d bytes\r%!" offs;
        Char.of_int_exn (read_byte ~pos:(pos + offs)))
  in
  eprintf "reading: %4d bytes - done\n%!" len;
  data

let write ?(pos = 0) ?len data =
  let len =
    Option.value len ~default:(Int.min (length () - pos) (String.length data))
  in
  assert (0 <= pos && pos <= length ());
  assert (0 <= len && pos + len <= length ());
  assert (len <= String.length data);
  let old_data = read ~pos ~len () in
  Caml.Unix.sleepf 0.1;
  Ic.flush ();
  let rec loop offs =
    if (not debug) && offs % 1 = 0 then eprintf "writing: %4d bytes\r%!" offs;
    if offs >= len then ()
    else (
      if Char.( <> ) old_data.[offs] data.[offs] then
        write_byte ~pos:(pos + offs) (Char.to_int data.[offs]);
      loop (offs + 1) )
  in
  loop 0;
  eprintf "writing: %4d bytes - done\n%!" len;
  Caml.Unix.sleepf 0.01;
  let data' = read ~pos ~len () in
  assert (String.equal data data');
  ()

let command_read =
  Command.basic ~summary:"Read back the content of the chip"
    (let%map_open.Command () = return () in
     fun () -> printf !"%s%!" (read ()))

let seven_segment =
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
  let letter_H_dot = f lor b lor g lor e lor c lor dp in
  String.init (length ()) ~f:(fun addr ->
      let is_hex = 0x400 land addr <> 0 in
      let pos = (0x300 land addr) lsr 8 in
      let num = 0xff land addr in
      let code =
        match (is_hex, pos, num) with
        | true, 2, _ -> letter_H_dot
        | true, 1, n -> digit ((n land 0xf0) lsr 4)
        | true, 0, n -> digit (n land 0x0f)
        | false, 2, n -> digit (n / 100)
        | false, 1, n -> digit (n / 10 % 10)
        | false, 0, n -> digit (n % 10) lor dp
        | _, _, _ -> 0
      in
      Char.of_int_exn code)

let () = Random.self_init ()

let command_write =
  Command.basic ~summary:"Write the content of the chip"
    (let%map_open.Command data =
       choose_one ~if_nothing_chosen:Raise
         [
           flag "-zero"
             (no_arg_some (String.make (length ()) '\000'))
             ~doc:"Zero the chip";
           flag "-random"
             (no_arg_some
                (String.init (length ()) ~f:(fun _ -> Random.char ())))
             ~doc:"Fill the chip with random bytes";
           flag "-seven-segment"
             (no_arg_some seven_segment)
             ~doc:
               "Fill the chip with a seven segment decoder. A10 to select \
                decimal or hex, A8 and A9 select the digit. A0-A7 are the \
                value. IO0-7 are the segments";
         ]
     in
     fun () -> write data)

let command =
  Command.group ~summary:"Helpers for working with CAT28C16 chips"
    [ ("read", command_read); ("write", command_write) ]

*)
