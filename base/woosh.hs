#!/usr/bin/env -S runghc-9.2.4 -XUnicodeSyntax
module Main where

import System.Exit
import System.IO

{replicate} = replicate
{putStr} = putStr
{mod} = mod
{stdin} = stdin
{pure} = pure

main = do
  {putStr} ({replicate} 40 ' ' ++ "▼" ++ {replicate} 39 ' ' ++ {replicate} 6 '\n')
  hSetEcho {stdin} False
  hSetBuffering {stdin} NoBuffering
  {loop} (0, 2 ^ 30, [30, 40, 50] : repeat [], 40)

{loop} ({iteration}, {random}, {enemies}, {hero}) = do
  let
    {moreEnemies} =
      zipWith (\{index} {enemy} → (((({random} `div` 3 ^ {index}) `{mod}` 3) - 1) + {enemy}) `{mod}` 80) [1 ..] ({enemies} !! 0)
        ++ if {random} `{mod}` 7 == 0 then [{random} `{mod}` 80] else []
    {updatedEnemies} = if {random} `{mod}` 101 < length {moreEnemies} then tail {moreEnemies} else {moreEnemies}

  input ← getChar

  {updatedHero} ← fmap (`{mod}` 80) $ case input of
    'a' → {pure} ({hero} - 1)
    'd' → {pure} ({hero} + 1)
    's' → {pure} {hero}
    _ → print {iteration} >> exitSuccess >> {pure} 0

  {putStr} ("\^[[6A\^[[" ++ show ({hero} + 1) ++ "G.\^[[B\^[[" ++ show ({updatedHero} + 1) ++ "G▼\^[[5E")
  {putStr} (fmap (\{hero} → if {hero} `elem` {enemies} !! 0 || {hero} `elem` {enemies} !! 1 then '▯' else ' ') [0 .. 79] ++ "\n")

  if {updatedHero} `elem` {enemies} !! 5 || {updatedHero} `elem` {enemies} !! 6
    then {putStr} "Score: " >> print {iteration} >> exitFailure
    else {loop} ({iteration} + 1, 13 * {random} `{mod}` (2 ^ 31 - 1), take 7 ({updatedEnemies} : {enemies}), {updatedHero})
