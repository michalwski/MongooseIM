#!/bin/bash
set -x
set -e
set -o pipefail
TAG="1.5"
DATE=2015-01-27
pushd $TAG/_posts
for F in *.md; do
    mv "$F" "$DATE-$F"
done
popd
