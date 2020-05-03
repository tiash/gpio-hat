#!/bin/bash

upper_case() {
  echo "$@" | tr "[:lower:]" "[:upper:]"
}
module_name() {
  echo "$(upper_case "${1:0:1}")${1:1}"
}

cat <<EOF
open! Core

let all =
  [ 
EOF

for single_model in $(grep -le "^let model =" *.ml); do
  echo "    [ $(module_name "${single_model%.ml}").model ] ;"
done
for many_models in $(grep -le "^let models =" *.ml); do
  echo "    $(module_name "${many_models%.ml}").models ;"
done

cat <<EOF
  ]
  |> List.concat
EOF
