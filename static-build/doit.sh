#!/bin/sh

docker build --rm -t hops-musl .
docker run --name hops hops-musl
docker cp hops:/hops/hops .
docker rm hops
