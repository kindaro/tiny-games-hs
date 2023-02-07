# Tiny games in haskell

Inspired by the "write a game in 10 lines of basic" competition
found by f-a at https://www.homeputerium.de (scroll to the bottom for their rules in english).
Here are suggested rules for tiny haskell games!

The first Haskell tiny games contest runs through February 2023.
The prize is.. glory.
Submit your entries now, here or in the #haskell-game chat
([Matrix](https://matrix.to/#/#haskell-game:matrix.org) / [IRC](https://web.libera.chat/#haskell-game)).
Here are the categories:

## gam-10-80-hs-base ("base")

Rules:

- Write a playable game in haskell, in one file containing up to 10 lines of up to 80 characters each.
- This can be a stack, cabal, or runghc script. runghc scripts must note the ghc version tested with.
- Only the base package may be used.
- The game should work, ideally on all major platforms.
- The game must be accompanied by a small screenshot.

Entries:

- [guess1 <img src="gam-10-80-hs-base/guess1.png" height=50>](gam-10-80-hs-base/guess1.hs)

## gam-10-80-hs-default ("default")

Rules:

- As above, but a second file named Import.hs may be used, to gather imports and re-exports (only).
- All packages installed by default with the tested ghc version may be used.

Entries:

- [guess2 <img src="gam-10-80-hs-default/guess2.png" height=50>](gam-10-80-hs-default/guess2.hs)

## gam-10-80-hs-hackage ("hackage")

Rules:

- As above, but all packages released on Hackage may be used.

Entries:
