open Core

module Make_bits (T : sig
  val bits : Gpio_hat.Pin.t list
end) : sig
  val bits : int

  type t [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  val arg_type : t Command.Arg_type.t

  val to_string : t -> string

  val of_int : int -> t

  val to_int : t -> int

  val get : unit -> t

  val will_get : unit -> unit

  val set : t -> unit
end = struct
  let pins = List.rev T.bits

  let bits = List.length pins

  module T : sig
    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t

    val of_int : int -> t

    val to_int : t -> int
  end = struct
    include Int

    let to_int = Fn.id

    let min_value = 0

    let max_value = (1 lsl bits) - 1

    let () = assert (max_value <> 0)

    let () = assert (max_value > min_value)

    let () = assert (popcount max_value = bits)

    let of_int n =
      if n <> (n land max_value) then
        raise_s
          [%message
            "Value is out of range, unexpected pins"
              (min_value : int)
              (n : int)
              (max_value : int)];
      n
  end

  include T

  let to_string = sprintf !"%{sexp:t}"

  let arg_type = Command.Arg_type.(map Export.int ~f:of_int)

  let will_get () = List.iter pins ~f:(fun pin -> Gpio_hat.set_mode pin Input)

  let get () =
    List.foldi pins ~init:0 ~f:(fun ix acc pin ->
        if Gpio_hat.get pin then acc lor (1 lsl ix) else acc)
    |> of_int

  let set n =
    let n = to_int n in
    List.iteri pins ~f:(fun ix pin ->
        Gpio_hat.set pin (Int.(<>) (n land (1 lsl ix)) 0))
end

module Make (T : sig
  val name : string

  val config_pins :
    cs:bool -> oe:bool -> we:bool -> (Gpio_hat.Pin.t * bool) list

  val addr : Gpio_hat.Pin.t list

  val data : Gpio_hat.Pin.t list

  val read_latency : Time.Span.t

  val write_pulse : Time.Span.t

  val write_timeout : Time.Span.t

  val write_finished : expected:int -> actual:int -> bool
end) : sig
  module Addr : sig
    val bits : int

    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t

    val of_int : int -> t

    val to_int : t -> int
  end

  module Data : sig
    val bits : int

    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t

    val of_int : int -> t

    val to_int : t -> int
  end

  val capacity : int

  val read_word : Addr.t -> Data.t

  val write_word : Addr.t -> Data.t -> unit

  val read : ?pos:Addr.t -> ?length:int -> unit -> Data.t list

  val write : ?pos:Addr.t -> Data.t list -> unit

  val command : Command.t
end = struct
  module Addr = Make_bits (struct
    let bits = T.addr
  end)

  module Data = Make_bits (struct
    let bits = T.data
  end)

  let capacity = 1 lsl Addr.bits

  let config_pins ~cs ~we ~oe =
    List.iter (T.config_pins ~cs ~we ~oe) ~f:(fun (pin, value) ->
        Gpio_hat.set pin value)

  let ensure_init () = 
    Addr.set (Addr.of_int 0);
    config_pins ~cs:false ~we:false ~oe:false;
    Gpio_hat.flush ();
    Caml.Unix.sleepf (Time.Span.to_sec T.write_timeout)
;;
    

  let read_word addr =
    (* ensure_init (); *)
    config_pins ~cs:true ~we:false ~oe:false;
    Addr.set addr;
    Data.will_get ();
    Gpio_hat.flush ();
    config_pins ~cs:true ~we:false ~oe:true;
    Gpio_hat.flush ();
    Caml.Unix.sleepf (Time.Span.to_sec T.read_latency);
    Gpio_hat.flush ();
    let data = Data.get () in
    Gpio_hat.flush ();
    config_pins ~cs:true ~we:false ~oe:false;
    Gpio_hat.flush ();
    data

  let write_word addr data =
    (* ensure_init (); *)
    let old_data = read_word addr in
    if Data.equal data old_data then ()
    else (
      config_pins ~cs:true ~we:false ~oe:false;
      Addr.set addr;
      Data.set data;
      Gpio_hat.flush ();
      config_pins ~cs:true ~we:true ~oe:false;
      Gpio_hat.flush ();
      Caml.Unix.sleepf (Time.Span.to_sec T.write_pulse);
      config_pins ~cs:true ~we:false ~oe:false;
      Gpio_hat.flush ();
      let write_start = Time.now () in
      let rec wait_loop () =
        let now = Time.now () in
        (* Caml.Unix.sleepf (Time.Span.to_sec T.read_latency); *)
        let new_data = read_word addr in
        if
          T.write_finished ~expected:(Data.to_int data)
            ~actual:(Data.to_int new_data)
        then (
          if not (Data.equal data new_data) then
            raise_s
              [%message
                "Write failed at"
                  (addr : Addr.t)
                  (old_data : Data.t)
                  (data : Data.t)
                  (new_data : Data.t)];
          () )
        else if
          Time.Span.( > ) (Time.diff now write_start) T.write_timeout
        then
          raise_s
            [%message
              "Write failed to complete in a reasonable amount of time"
                (addr : Addr.t)
                (old_data : Data.t)
                (data : Data.t)
                (new_data : Data.t)
                ~time:(Time.diff (Time.now ()) write_start : Time.Span.t)]
        else wait_loop ()
      in
      wait_loop () )

  let read ?(pos = Addr.of_int 0) ?length () =
    ensure_init (); 
    let length = Option.value length ~default:(capacity - Addr.to_int pos) in
    assert (Addr.to_int pos + length <= capacity);
    List.init length ~f:(fun ix ->
        read_word (Addr.of_int (Addr.to_int pos + ix)))

  let write ?(pos = Addr.of_int 0) data =
    ensure_init (); 
    let length = List.length data in
    assert (Addr.to_int pos + length <= capacity);
    List.iteri data ~f:(fun ix data ->
        let addr = Addr.of_int (Addr.to_int pos + ix) in
        write_word addr data);
    List.iteri data ~f:(fun ix data ->
        let addr = Addr.of_int (Addr.to_int pos + ix) in
        let new_data = read_word addr in
        if not (Data.equal data new_data) then
          raise_s
            [%message
              "Write failed at"
                (pos : Addr.t)
                (ix : int)
                (addr : Addr.t)
                (data : Data.t)
                (new_data : Data.t)])

  module Data_source = Data_source.Make (Data)

  module Range : sig
    type t [@@deriving sexp_of]

    val pos : t -> Addr.t

    val length : ?max_length:int -> t -> int

    val addr : t -> int -> Addr.t

    val iter : t -> f:(Addr.t -> unit) -> unit

    val param : t Command.Param.t
  end = struct
    let capacity ~pos = capacity - Addr.to_int pos

    type t = { pos : Addr.t; length : int option } [@@deriving sexp_of]

    let param =
      let%map_open.Command pos =
        flag_optional_with_default_doc "start" Addr.arg_type [%sexp_of: Addr.t]
          ~doc:"ADDR Start address" ~default:(Addr.of_int 0)
      and length =
        flag "length" (optional int)
          ~doc:"LENGHT Number of words of data to read"
      in
      Option.iter length ~f:(fun length ->
          let capacity = capacity ~pos in
          if length > capacity then
            raise_s
              [%message
                "length is larger than available capacity"
                  (length : int)
                  (capacity : int)]);
      { pos; length }

    let pos t = t.pos

    let length ?max_length { pos; length } =
      let capacity = capacity ~pos in
      let max_length =
        match max_length with
        | None -> capacity
        | Some max_length -> Int.min capacity max_length
      in
      match length with
      | None -> max_length
      | Some length ->
          if length > max_length then
            raise_s
              [%message
                "length is larger than max" (length : int) (max_length : int)];
          length

    let addr t ix =
      let length = length t in
      if ix > length then
        raise_s
          [%message "offset out of bounds" (t : t) (ix : int) (length : int)];
      Addr.of_int (Addr.to_int t.pos + ix)

    let iter t ~f =
      let length = length t in
      let rec iter_loop ix =
        if ix >= length then ()
        else (
          f (Addr.of_int (Addr.to_int t.pos + ix));
          iter_loop (ix + 1) )
      in
      iter_loop 0
  end

  let read_command =
    Command.basic ~summary:"Read data"
      (let%map_open.Command range = Range.param
       and format =
         choose_one
           ~if_nothing_chosen:(Default_to `Binary)
           [
             flag "-binary"
               (no_arg_some `Binary)
               ~doc:" Output data in binary format";
             flag "-words"
               (no_arg_some `Words)
               ~doc:" Output data in words (1 decimal word of data per line)";
           ]
       in
       fun () ->
    ensure_init (); 
         Range.iter range ~f:(fun addr ->
             let data = read_word addr in
             match format with
             | `Binary -> printf "%c%!" (Char.of_int_exn (Data.to_int data))
             | `Words -> printf !"%{Data}%!\n" data))

  let data_param =
    let%map_open.Command source = Data_source.param in
    fun range ->
      let source = source (Range.length range) in
      let length = Range.length range ~max_length:(List.length source) in
      if length > List.length source then raise_s [%message ""];
      List.take source length

  let check_command =
    Command.basic ~summary:"Check data"
      (let%map_open.Command range = Range.param
       and expected_data = data_param in
       let expected_data = expected_data range in
       fun () ->
    ensure_init (); 
         let errors = ref false in
         List.iteri expected_data ~f:(fun ix expected_data ->
             let addr = Range.addr range ix in
             let actual_data = read_word addr in
             if Data.( <> ) expected_data actual_data then (
               errors := true;
               eprint_s
                 [%message
                   "Unexpected data"
                     (range : Range.t)
                     (ix : int)
                     (addr : Addr.t)
                     (expected_data : Data.t)
                     (actual_data : Data.t)] ));
         if !errors then raise_s [%message "Unexpected data found"])

  let write_command =
    Command.basic ~summary:"Check data"
      (let%map_open.Command range = Range.param and data = data_param in
       let data = data range in
       fun () ->
    ensure_init (); 
write ~pos:(Range.pos range) data)

  let command =
    Command.group
      ~summary:("Helpers for reading/flashing " ^ T.name ^ " EEPROMs")
      [
        ("read", read_command);
        ("check", check_command);
        ("write", write_command);
      ]
end
