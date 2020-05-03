let all =
  [
    (*$
      open! Core;;
      printf "\n    ";;
      Sys.ls_dir "."
      |> List.filter_map ~f:(String.chop_suffix ~suffix:".ml")
      |> List.filter_map ~f:(String.chop_prefix ~prefix:"model_")
      |> List.filter ~f:(fun n -> not (String.contains n '.'))
      |> List.map ~f:String.capitalize
      |> List.iter ~f:(printf "Model_%s.model;\n    ")
    *)
    Model_7400.model;
    (*$*)
  ]
