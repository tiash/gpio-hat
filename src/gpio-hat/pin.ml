open Core

module T = struct
  type n = int [@@deriving compare]

  let all_of_n =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]

  type t = A of n | B of n [@@deriving compare, enumerate]

  let to_string = function A n -> sprintf "A%02d" n | B n -> sprintf "B%02d" n

  let sexp_of_t t = [%sexp (to_string t : string)]

  let of_string s =
    let s = String.uppercase s in
    match String.chop_prefix s ~prefix:"A" |> Option.map ~f:Int.of_string with
    | Some n ->
        assert (1 <= n && n <= 20);
        A n
    | None ->
        let n = String.chop_prefix_exn s ~prefix:"B" |> Int.of_string in
        assert (1 <= n && n <= 20);
        B n
end

include T
include Comparable.Make_plain (T)

let arg_type = Command.Arg_type.create of_string
