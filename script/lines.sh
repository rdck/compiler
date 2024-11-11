#! /bin/sh

wc -l $(git ls-files | grep -E "\.ml$|\.mli$|\.mll")
