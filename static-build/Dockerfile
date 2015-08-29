FROM nilcons/ghc-musl:stackage
MAINTAINER Anders Claesson

RUN cabal update
RUN cabal install hops --only-dependencies

COPY mk-hops.sh /usr/local/bin/

CMD /usr/local/bin/mk-hops.sh
