#!/bin/sh

docker build --rm -t hops-musl .
docker run -v /tmp/x:/tmp/x -it --rm hops-musl
cp /tmp/x/hops/hops .
