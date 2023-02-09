#!/usr/bin/env -S runghc-9.2.4 -XUnicodeSyntax
{ import System.IO
; import System.Exit
; main = do
  { putStr ("\^[[?25l" ++ replicate 40 ' ' ++ "▼" ++ replicate (39) ' ' ++ "\n\n\n\n\n\n")
  ; hSetEcho stdin False
  ; hSetBuffering stdin NoBuffering
  ; loop (0, 2^30, [30, 40, 50]: repeat [ ], 40)
  }
; loop state@(i, r, es, x) = do
  { let
      { est = zipWith (\ j e → ((((r `div` 3^j) `mod` 3) - 1) + e) `mod` 80) [1..] (es !! 0)
            ++ if r `mod` 7 == 0 then [r `mod` 80] else [ ]
      ; es' = if r `mod` 101 < length est then tail est else est
    }
  ; input ← getChar
  ; x' ← fmap (`mod` 80) $ case input of
    { 'a' → pure (x - 1)
    ; 'd' → pure (x + 1)
    ; 's' → pure x
    ; _ → print i >> exitSuccess >> pure 0
    }
  ; putStr ("\^[[6A\^[[" ++ show (x + 1) ++ "G.\^[[B\^[[" ++ show (x' + 1) ++ "G▼\^[[5E")
  ; putStrLn (fmap (\x → if x `elem` es !! 0 || x `elem` es !! 1 then '▯' else ' ') [0..79])
  ; if x' `elem` es !! 5 || x' `elem` es !! 6
      then print i >> exitFailure
      else loop (i + 1, 13 * r `mod` (2^31 - 1), take 7 (es':es), x')
  }
}
