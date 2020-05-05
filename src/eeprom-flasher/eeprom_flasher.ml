open! Core

module Eeprom_28C16 = Flasher.Make (struct
  let name = "28C16"

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

  let write_pulse = Time.Span.of_ns 100. (* spec says 100 to 1000 *)

  let write_timeout = Time.Span.of_ms 2. (* spec says 1 *)

  let write_finished ~expected ~actual = expected land 0x80 = actual land 0x80
end)

let command = Command.group ~summary:"" [ ("28C16", Eeprom_28C16.command) ]

