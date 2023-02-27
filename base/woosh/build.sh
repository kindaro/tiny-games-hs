#!/usr/bin/env sh

export depth='6'
export avatar='▼'
export enemy='▯'
export blank=' '
export map='2 ^ 30'
export enemies='[30, 40, 50]'
export odds_of_enemy_death_numerator='1'
export odds_of_enemy_death_denominator='101'
export odds_of_new_enemy_numerator='1'
export odds_of_new_enemy_denominator='7'

export before_depth="$(($depth - 1))"
export after_depth="$(($depth + 1))"

envsubst < `dirname $0`/woosh.hs > `dirname $0`/woosh.envsubst.hs
cabal ./minify.hs --source `dirname $0`/woosh.envsubst.hs --target `dirname $0`/woosh.minified.hs
chmod +x `dirname $0`/woosh.minified.hs
