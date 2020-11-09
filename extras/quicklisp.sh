#!/bin/bash

[[ -d $HOME/quicklisp ]] && exit 0  # nothing to be done

CURDIR=$(pwd)
tmpdir=$(mktemp -d)
cd $tmpdir

curl -O https://beta.quicklisp.org/quicklisp.lisp

sbcl --load quicklisp.lisp --no-linedit 

cd $CURDIR
echo rm -rf $tmpdir

