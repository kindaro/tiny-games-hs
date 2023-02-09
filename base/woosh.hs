#!/usr/bin/env -S runghc-9.2.4 -XUnicodeSyntax
{ import System.IO
; import System.Exit
; main = do
  { putStr ("\^[[?25l" ++ replicate 40 ' ' ++ "v" ++ replicate (39) ' ' ++ "\n\n\n\n")
  ; hSetEcho stdin False
  ; hSetBuffering stdin NoBuffering
  ; loop (0, 2^30, [20, 30, 40, 50, 60]: repeat [], 40)
  }
; loop state@(i, r, es, x) = do
  { let {es' = zipWith (\j e → ((((r`div`3^j)`mod`3) - 1)+e) `mod` 80) [1..] (es!!0)}
  ; input ← getChar
  ; x' ← fmap (`mod` 80) $ case input of
    { 'a' → pure (x-1)
    ; 'd' → pure (x+1)
    ; 's' → pure x
    ; _ → print i >> exitSuccess >> pure 0
    }
  ; putStr("\^[[4A\^[["++show(x+1)++"G.\^[[B\^[["++show(x'+1)++"Gv\^[[3E")
  ; putStrLn (fmap (\x → if x `elem` es!!0 || x `elem` es!!1 then '*' else ' ') [0..79])
  ; if x' `elem` es!!3 || x' `elem` es!!4
      then print i >> exitFailure
      else loop (i + 1, 13 * r `mod` (2^31-1), take 5 (es':es), x')
  }
}
