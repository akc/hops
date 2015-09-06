#!/bin/sh

cd /tmp/x

if cd hops
then
    git pull
else
    git clone https://github.com/akc/hops.git
    cd hops
fi

ghc --make -threaded -O2 -with-rtsopts=-N -optl-static hops.hs
strip -s hops
