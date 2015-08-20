#!/bin/sh

docker build --rm -t gfscript-musl .
docker run -v /tmp/x:/tmp/x -it --rm gfscript-musl
cp /tmp/x/gfscript/gfscript .
