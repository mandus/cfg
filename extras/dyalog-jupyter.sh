#!/bin/bash

# Add a dyalog apl kernel for jupyter notebook. Assumption is that jupyter notebook/lab already is present on system
#

# Do nothing if already present:
[[ -d $HOME/software/src/dyalog-jupyter-kernel ]] && exit 0

CURDIR=$(pwd)

cd $HOME/software/src
git clone git@github.com:Dyalog/dyalog-jupyter-kernel.git
cd dyalog-jupyter-kernel
./install.sh

cd $CURDIR
