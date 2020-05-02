
let all = [
(*$
let open! Core in
Sys.ls_dir "."
|> List.filter_map ~f:(String.chop_suffix name ~suffix:".mli")
|> List.map ~f:String.capitalize
|> List.iter ~f:(printf "%s.model;\n")
$*)
(*$*)
]

