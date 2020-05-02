open Core

let or_error_list_iteri l ~f =
  let rec loop i = function
    | [] -> Ok ()
    | hd :: tl -> (
        match f i hd with Ok () -> loop (i + 1) tl | Error _ as err -> err )
  in
  loop 0 l

let test_trace' trace =
  or_error_list_iteri trace ~f:(fun step state ->
      Map.iteri state ~f:(fun ~key:pin ~data ->
          match data with
          | Trace.Pin.Not_connected -> Gpio_hat.set_mode pin Not_connected
          | Input value | Constant value ->
              Gpio_hat.set_mode pin Output;
              Gpio_hat.set pin value
          | Output _ | Mismatch _ -> Gpio_hat.set_mode pin Input);
      Gpio_hat.flush ();
      let state' =
        Map.mapi state ~f:(fun ~key:pin ~data ->
            match data with
            | Input _ | Constant _ | Not_connected -> data
            | Output value | Mismatch value ->
                let pin = Gpio_hat.get pin in
                if Bool.equal pin value then data else Mismatch value)
      in
      if [%compare.equal: Trace.State.t] state state' then Ok ()
      else
        let len = List.length trace in
        let trace = List.take trace step @ [ state' ] in
        Error
          (sprintf
             !"Unexpected IC Behaviour at step %d of %d.\n\
               %{Trace#ansi}"
             step len trace))

let test_m m =
  Gpio_hat.reset ();
  Sequence.fold ~init:0 (Trace.traces m) ~f:(fun errors transcript ->
      match test_trace' transcript with
      | Ok () ->
          eprintf ".%!";
          errors
      | Error error ->
          eprintf !"\n%s\n%!" error;
          errors + 1)
  |> function
  | 0 ->
      Gpio_hat.reset ();
      Ok ()
  | error_count ->
      Gpio_hat.reset ();
      Error (sprintf "%d errors detected while testing IC" error_count)

let test_trace trace =
  Gpio_hat.reset ();
  let res = test_trace' trace in
  Gpio_hat.reset ();
  res
