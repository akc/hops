#!/bin/sh

cd /tmp/x

if cd hops
then
    git pull
else
    git clone https://github.com/akc/hops.git
    cd hops
fi

cabal update
cabal install --only-dependencies
cabal build

ghc --make -threaded -O2 -with-rtsopts=-N -optl-static -fforce-recomp \
    -optP-include -optPdist/build/autogen/cabal_macros.h hops.hs

strip -s hops
