#!/usr/bin/env sh

export depth='6'
export avatar='▼'
export map='2 ^ 30'
export enemies='[30, 40, 50]'

envsubst < `dirname $0`/woosh.hs > `dirname $0`/woosh.envsubst.hs
cabal ./minify.hs --source `dirname $0`/woosh.envsubst.hs --target `dirname $0`/woosh.minified.hs
chmod +x `dirname $0`/woosh.minified.hs
