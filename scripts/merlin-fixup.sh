#!/bin/bash

# fixup local .merlin files after building in Docker

# local OPAM switch
OPAM_SWITCH=4.08.0
# Nominal OCaml version in the Docker container
OCAML_VERSION=4.08

for file in `find src -name .merlin`
do
    cp -p $file $file.SAVE
    sed -i.bak s+.opam/$OCAML_VERSION/+.opam/$OPAM_SWITCH/+g $file
    sed -i.bak s+/home/opam/.opam/+$HOME/.opam/+g $file
    sed -i.bak s+/home/opam/app/src/_build/+$(git rev-parse --show-toplevel)/src/_build/+g $file
done
