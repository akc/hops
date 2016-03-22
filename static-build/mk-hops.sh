#!/bin/sh

if cd hops 2>/dev/null
then
    git fetch --all
else
    git clone https://github.com/akc/hops.git
    cd hops
fi
git reset --hard origin/master

echo "constraints: http-conduit==2.1.7" >cabal.config

cabal update
cabal install --only-dependencies --force-reinstalls

# This will give an error if pandoc isn't installed, but that is fine as
# we only call build to generate dist/build/autogen/cabal_macros.h. Is
# there some cleaner solution for this?
cabal configure
cabal build

ghc --make -threaded -O2 -with-rtsopts=-N -optl-static -fforce-recomp \
    -optP-include -optPdist/build/autogen/cabal_macros.h hops.hs

strip -s hops
