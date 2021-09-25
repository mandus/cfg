#!/bin/bash

# scripted install of dotnet - required on arm64, but nice to have an uniform way
#

[[ -d $HOME/.dotnet ]] && exit 0

CURDIR=$(pwd)
tmpdir=$(mktemp -d)
cd $tmpdir

curl -O -L https://dot.net/v1/dotnet-install.sh
bash dotnet-install.sh -c 6.0

# Also install the VS debugger
#

if [[ ! -d $HOME/.vsdbg ]] ; then
	curl -O -L https://aka.ms/getvsdbgsh
	bash getvsdbgsh -v latest -l ~/.vsdbg
fi

cd $CURDIR
echo rm -rf $tmpdir
