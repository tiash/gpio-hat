open Core

module Make (Word : sig
  type t

  val of_int : int -> t
end) : sig
  val param : (int -> Word.t list) Command.Param.t
end = struct
  let read_param =
    let%map_open.Command input =
      choose_one ~if_nothing_chosen:Return_none
        [
          (let%map_open.Command file =
             flag "-file"
               (optional Filename.arg_type)
               ~doc:"FILE Data from file"
           in
           Option.map file ~f:In_channel.read_all);
          (let%map_open.Command stdin =
             flag "-stdin" no_arg ~doc:" Data from stdin"
           in
           if stdin then Some (In_channel.input_all In_channel.stdin) else None);
        ]
    in
    Option.map input ~f:(fun data length ->
        if String.length data < length then failwith "Not enough data";
        List.init length ~f:(fun ix -> Word.of_int (Char.to_int data.[ix])))

  let param =
    Command.Param.choose_one ~if_nothing_chosen:Raise
      ( read_param
      :: List.map Generators.all ~f:(fun (name, f) ->
             let%map_open.Command v =
               flag name no_arg ~doc:" Data from generator"
             in
             if v then Some (fun length -> List.map (f length) ~f:Word.of_int)
             else None) )
end
