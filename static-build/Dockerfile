FROM nilcons/ghc-musl:stackage
MAINTAINER Anders Claesson

RUN cabal update
RUN cabal install gfscript --only-dependencies

COPY mk-gfscript.sh /usr/local/bin/

CMD /usr/local/bin/mk-gfscript.sh
