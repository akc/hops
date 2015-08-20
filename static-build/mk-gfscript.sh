#!/bin/sh

cd /tmp/x

if cd gfscript
then
    git pull
else
    git clone https://github.com/akc/gfscript.git
    cd gfscript
fi

ghc --make -O2 -optl-static gfscript.hs
strip -s gfscript
