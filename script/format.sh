#! /bin/sh

ocamlformat -i $(git ls-files | grep -E "\.ml$|\.mli$")
