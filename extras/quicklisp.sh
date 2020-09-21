#!/bin/bash

[[ -d $HOME/quicklisp ]] && exit 0  # nothing to be done

CURDIR=$(pwd)
tmpdir=$(mktemp -d)
cd $tmpdir

curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp

sbcl --load quicklisp.lisp

cd $CURDIR
echp rm -rf $tmpdir

