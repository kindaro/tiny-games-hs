#!/usr/bin/env sh

export depth='30'
export avatar='▼'
export enemy='▯'
export blank=' '
export map='2 ^ 30'
export enemies='[30, 40, 50]'
export odds_of_enemy_death_numerator='10'
export odds_of_enemy_death_denominator='101'
export odds_of_new_enemy_numerator='7'
export odds_of_new_enemy_denominator='7'
export step_delay='(2*10^5)'

export before_depth="$(($depth - 1))"
export after_depth="$(($depth + 1))"

envsubst < `dirname $0`/swish.hs > `dirname $0`/swish.envsubst.hs
cabal ./minify.hs --source `dirname $0`/swish.envsubst.hs --target `dirname $0`/swish.minified.hs
chmod +x `dirname $0`/swish.minified.hs
