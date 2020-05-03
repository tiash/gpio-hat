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

for file in $(grep -le "^let model =" model_*.ml); do
  echo "    [ $(module_name "${file%.ml}").model ] ;"
done

cat <<EOF
  ]
  |> List.concat
EOF
