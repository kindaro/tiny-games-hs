module Main where

import Imports

main = playGame (Game tps initState logicFunction drawFunction quitFunction)

(⋮) = (,)
plus (y, x) (y', x') = ((y + y' - 1) `mod` columns_ + 1, (x + x' - 1) `mod` rows_ + 1)
minus (y, x) = (-y, -x)

type State = [Snake]
type Snake = (Int, (Int, Int), [(Int, Int)])

tps = 2

initState =
  [ (3, (0, -1), [10 ⋮ 2, 10 ⋮ 3, 10 ⋮ 4])
  , (5, (-1, 0), [20 ⋮ 67, 20 ⋮ 68, 21 ⋮ 68])
  ]

logicFunction ∷ GEnv → State → Event → State
logicFunction _ state Tick = (feed . fmap (wither . slither)) state
logicFunction _ state (KeyPress key) = fmap (_2 .~ characterToHeading key) state

-- logicFunction genv (avatar: enemies) (KeyPress char) = slither
slither ∷ Snake → Snake
slither (energy, heading, jaw : spine) = (energy, heading, jaw `plus` heading : jaw : spine)
wither ∷ Snake → Snake
wither snake@(energy, heading, jaw : spine) = if snake ^. _1 == 0 then snake & _3 %~ init else snake & _1 %~ pred

feed ∷ [Snake] → [Snake]
feed snakes = snakes & imapped %@~ mapping
 where
  mapping index snake = snake & _1 %~ (+ sum energies)
   where
    energies =
      [ snake' ^. _3 . to length
      | snake' ← snakes \\ [snake] & ix index . _3 %~ tail
      , snake ^. _3 . to head `elem` snake' ^. _3
      ]

characterToHeading ∷ Char → (Int, Int)
characterToHeading 'w' = (-1) ⋮ 0
characterToHeading 's' = 1 ⋮ 0
characterToHeading 'a' = (0, -1)
characterToHeading 'd' = 0 ⋮ 1

drawFunction genv (avatar : enemies) = foldr ((.) . drawSnake) (bold . drawSnake avatar) enemies (box rows_ columns_ '.')

quitFunction ∷ State → Bool
quitFunction = (^. to head . _3 . to null)

drawSnake ∷ Snake → Draw
drawSnake (energy, heading, jaw : spine) = foldr (\x → ((x % cell '•') .)) (jaw % cell '@') spine
