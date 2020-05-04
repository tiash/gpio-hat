open Core
open Ic_tester

let ic_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      List.concat_map Ic_tester_models.all ~f:(fun model ->
          List.filter
            (Model.name model :: Model.aliases model)
            ~f:(String.Caseless.is_prefix ~prefix:part)))
    (fun name ->
      List.find_exn Ic_tester_models.all ~f:(fun model ->
          List.exists
            (Model.name model :: Model.aliases model)
            ~f:(String.Caseless.equal name)))

let show_command =
  Command.basic ~summary:"List known chips"
    (let%map_open.Command chip = anon (maybe ("IC" %: ic_arg)) in
     fun () ->
       let describe model =
         let title = Model.to_string model in
         print_endline title;
         print_endline (String.map title ~f:(const '='));
         ( match Model.aliases model with
         | [] -> ()
         | _ :: _ as aliases ->
             print_endline ("also: " ^ String.concat ~sep:", " aliases) );
         print_endline (Model.to_string_pinout model);
         print_endline (Model.description model)
       in
       match chip with
       | Some chip -> describe chip
       | None -> List.iter Ic_tester_models.all ~f:describe)

let trace_command =
  Command.basic ~summary:"Print expected chip behaviour"
    (let%map_open.Command ic = anon ("IC" %: ic_arg) in
     fun () ->
       Trace.traces (Model.logic ic)
       |> Sequence.iteri ~f:(fun i trace ->
              (*      xx: 12345678901234567890 *)
              printf "== %04d ================\n" (i + 1);
              print_endline (Trace.to_string_ansi trace)))

let test_command =
  Command.basic ~summary:"Test expected chip behaviour"
    (let%map_open.Command ic = anon ("IC" %: ic_arg) in
     fun () ->
       match Monad_runner.test_m (Model.logic ic) with
       | Ok () -> eprintf !"\n\027[32;40mOK\027[0m - %{Model}\n" ic
       | Error error ->
           eprintf !"\n\027[31;40mError %s\027[0m\n" error;
           exit 1)

let batch_test_command =
  Command.basic ~summary:"Test expected chip behaviour repeatedly"
    (let%map_open.Command ic = anon ("IC" %: ic_arg) in
     fun () ->
       let step () =
         match Monad_runner.test_m (Model.logic ic) with
         | Ok () -> eprintf !"\n\027[97;42mOK - %{Model}\027[0m\n%!" ic
         | Error error -> eprintf !"\n\027[97;41mError %s\027[0m\n%!" error
       in
       let rec loop () =
         match In_channel.input_line In_channel.stdin with
         | None -> ()
         | Some _ ->
             step ();
             loop ()
       in
       loop ())

let command =
  Command.group ~summary:""
    [
      ("show", show_command);
      ("trace", trace_command);
      ("test", test_command);
      ("batch-test", batch_test_command);
    ]
