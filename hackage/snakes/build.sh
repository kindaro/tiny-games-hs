#!/usr/bin/env sh

export rows=80
export columns=25

cat `dirname $0`/snakes.template.hs |
    sed 's/\([a-z_]\+\)_\>/$\1/g' |
    envsubst > `dirname $0`/snakes.envsubst.hs
cabal ../../minify.hs --source `dirname $0`/snakes.envsubst.hs --target `dirname $0`/snakes.hs
