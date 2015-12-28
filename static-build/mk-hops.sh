#!/bin/sh

cd /tmp/x

if cd hops 2>/dev/null
then
    git fetch
else
    git clone https://github.com/akc/hops.git
    cd hops
fi
git reset --hard origin/master

echo "constraints: scientific ==0.3.4.4" >cabal.config

cabal update
cabal install --only-dependencies

# This will give an error if pandoc isn't installed, but that is fine as
# we only call build to generate dist/build/autogen/cabal_macros.h. Is
# there some cleaner solution for this?
cabal build

ghc --make -threaded -O2 -with-rtsopts=-N -optl-static -fforce-recomp \
    -optP-include -optPdist/build/autogen/cabal_macros.h hops.hs

strip -s hops
