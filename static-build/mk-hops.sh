#!/bin/sh

cd /tmp/x

if cd hops
then
    git pull
else
    git clone https://github.com/akc/hops.git
    cd hops
fi

ghc --make -O2 -optl-static hops.hs
strip -s hops
