#!cabal
{- cabal:
default-language: GHC2021
default-extensions: BlockArguments DataKinds LambdaCase MultiWayIf OverloadedStrings RecordWildCards TypeFamilies UnicodeSyntax ViewPatterns
build-depends: base, containers, ghc, optparse-generic, text, mtl
ghc-options: -Wall -Wextra -Wno-unticked-promoted-constructors -Wunused-packages
-}

import Control.Applicative (Alternative ((<|>)), empty)
import Control.Applicative qualified as Applicative
import Control.Monad.State
import Data.Bifunctor
import Data.Char
import Data.Containers.ListUtils qualified as ListUtils
import Data.List
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import Options.Generic
import System.Exit

-- | Categorial dual of `either id id`.
fork ∷ (input → left) → (input → right) → input → (left, right)
fork function gunction = \input → (function input, gunction input)

for ∷ Functor functor ⇒ functor α → (α → β) → functor β
for = flip fmap

data Command = Command {source, target ∷ FilePath} deriving (Generic, Show)
instance ParseRecord Command

-- | These tokens do not require any white space around them.
specialTokens ∷ [Text]
specialTokens = ["(", ",", ")", "{", ";", "}", "[", "]", "←", "→", "<-", "->", "∷", "::", "\\"]

isAllowedInIdentifier ∷ Char → Bool
isAllowedInIdentifier = or . flip fmap [isAlphaNum, (== '_'), (== '\'')] . flip ($)

-- | Take the longest prefix that, as a whole, satisfies the condition.
cut ∷ ([α] → Bool) → [α] → ([α], [α])
cut fit list = (last . filter (fit . fst)) (zipWith splitAt [0 ..] (replicate (length list + 1) list))

defaultParserOptions ∷ ParserOpts
defaultParserOptions = mkParserOpts EnumSet.empty EnumSet.empty False False False False

defaultRealSourceLocation ∷ RealSrcLoc
defaultRealSourceLocation = mkRealSrcLoc "" 1 1

defaultLexer ∷ StringBuffer → ParseResult [Located Token]
defaultLexer stringBuffer = lexTokenStream defaultParserOptions stringBuffer defaultRealSourceLocation

-- | Try and tear the substring corresponding to a token out from the source code.
tearOut ∷ Text → Located Token → Maybe (Text, Token)
tearOut text (L realSrcSpan token) =
  let
    textLines = Text.lines text ++ repeat ""
    textLine = textLines !! (srcLocLine start - 1)
    (start, end) = case realSrcSpan of
      RealSrcSpan ((realSrcSpanStart `fork` realSrcSpanEnd) → spanTuple) _ → spanTuple
      UnhelpfulSpan _ → error "Impossible: We parse from file."
   in
    if srcLocLine start /= srcLocLine end
      then Nothing -- We expect that no token is spread across two or more lines.
      else Just ((Text.take (srcLocCol end - srcLocCol start) . Text.drop (srcLocCol start - 1)) textLine, token)

-- | Ask GHC to split source code into tokens.
lexify ∷ Text → Maybe [(Text, Token)]
lexify sourceCode =
  let textToParseResult = defaultLexer . stringToStringBuffer . Text.unpack
   in case textToParseResult sourceCode of
        POk _ tokens → traverse (tearOut sourceCode) tokens
        PFailed _ → Nothing

data Stripes (done ∷ Bool) black white where
  Black ∷ black → Stripes False black white → Stripes True black white
  White ∷ white → Stripes True black white → Stripes False black white
  Done ∷ black → Stripes True black white

instance Bifunctor (Stripes any) where
  bimap function gunction (Black black stripes) = Black (function black) (bimap function gunction stripes)
  bimap function gunction (White white stripes) = White (gunction white) (bimap function gunction stripes)
  bimap function _ (Done black) = Done (function black)

flatten ∷ Stripes any black white → [Either black white]
flatten (Black black stripes) = Left black : flatten stripes
flatten (White white stripes) = Right white : flatten stripes
flatten (Done black) = [Left black]

data Room = Love | Hate | Indifference deriving (Show, Eq, Ord)

loves, hates, isIndifferentTowards ∷ black → Stripes True black Room → Stripes True black Room
black `loves` stripes = Black black (White Love stripes)
black `hates` stripes = Black black (White Hate stripes)
black `isIndifferentTowards` stripes = Black black (White Indifference stripes)

getBlack ∷ Stripes True black white → black
getBlack (Black black _) = black
getBlack (Done black) = black

tokensToStripes ∷ NonEmpty (Token, Text) → Stripes True (Token, Text) Room
tokensToStripes = fix \recurse → \(token :| tokens) → case nonEmpty tokens of
  Nothing → Done token
  Just (recurse → stripes) → case (token, stripes) of
    ((ITat, _), _) → token `loves` stripes
    (_, (Black (ITat, _) _)) → token `loves` stripes
    ((_, this), (getBlack → (_, that))) →
      if
          | any (`elem` specialTokens) [this, that] → token `isIndifferentTowards` stripes
          | Text.null this || Text.null that → token `isIndifferentTowards` stripes
          | ((== '"') . Text.last) this || ((== '"') . Text.head) that → token `isIndifferentTowards` stripes
          | (isAllowedInIdentifier . Text.last) this == (isAllowedInIdentifier . Text.head) that → token `hates` stripes
          | otherwise → token `isIndifferentTowards` stripes

data Parser token output = Parser {parse ∷ [token] → [([token], output)]}
uncurryParser ∷ (firstOutput → Parser token nextOutput) → ([token], firstOutput) → [([token], nextOutput)]
uncurryParser makeParser (input, firstOutput) = parse (makeParser firstOutput) input
instance Functor (Parser input) where fmap function (Parser parser) = Parser ((fmap . fmap . fmap) function parser)
instance Applicative (Parser input) where
  pure output = Parser \inputs → [(inputs, output)]
  (<*>) = ap
instance Monad (Parser input) where
  firstParser >>= makeNextParser = Parser \firstInputs → parse firstParser firstInputs >>= uncurryParser makeNextParser
instance Alternative (Parser input) where
  empty = Parser do const []
  thisParser <|> thatParser = Parser \input →
    let
      thisOutput = parse thisParser input
      thatOutput = parse thatParser input
     in
      thisOutput <> thatOutput
instance MonadPlus (Parser input)

class (Monad parser, Alternative parser) ⇒ MonadParser parser where
  type Input parser
  foresee ∷ parser output → parser output
  orElse ∷ parser output → parser output → parser output
  match ∷ (Input parser → Bool) → parser (Input parser)
  matchMaybe ∷ (Input parser → Maybe output) → parser output
  close ∷ parser ()

instance MonadParser (Parser token) where
  type Input (Parser token) = token
  foresee parser = Parser \input → for (parse parser input) \(_, output) → (input, output)
  thisParser `orElse` thatParser = Parser \input →
    let thisOutput = parse thisParser input
     in if null thisOutput then parse thatParser input else thisOutput
  match matching = Parser \case
    (token : tokens) | matching token → [(tokens, token)]
    _ → []
  matchMaybe making = Parser \case
    (token : tokens) → maybe [] (pure . (tokens,)) (making token)
    _ → []
  close = Parser \input → if null input then [([], ())] else []

instance MonadParser (StateT state (Parser token)) where
  type Input ((StateT state (Parser token))) = Input (Parser token)
  foresee statefulParser = do
    stateOfParser ← get
    let parser = evalStateT statefulParser stateOfParser
    lift do foresee parser
  (StateT thisStatefulParser) `orElse` (StateT thatStatefulParser) = StateT \stateOfParser →
    let
      Parser thisParser = thisStatefulParser stateOfParser
      Parser thatParser = thatStatefulParser stateOfParser
     in
      Parser \input →
        let thisParseResult = thisParser input
         in if null thisParseResult then thatParser input else thisParseResult
  match = fmap lift match
  matchMaybe = fmap lift matchMaybe
  close = lift close

runParser ∷ Parser token output → [token] → Maybe output
runParser parser input = case parse parser input of
  [([], output)] → Just output
  _ → Nothing

fromLeft ∷ Either left right → Maybe left
fromLeft (Left left) = Just left
fromLeft (Right _) = Nothing

fromRight ∷ Either left right → Maybe right
fromRight (Right right) = Just right
fromRight (Left _) = Nothing

parseStripes
  ∷ (Monoid monoid, MonadParser monad)
  ⇒ monad monoid
  → monad monoid
  → monad monoid
parseStripes parseToken parseRoom = do
  firstToken ← parseToken
  leftovers ← Applicative.many do
    room ← parseRoom
    token ← parseToken
    pure [room, token]
  pure do mconcat (firstToken : concat leftovers)

try ∷ MonadParser parser ⇒ parser output → parser (Maybe output)
try this = fmap Just this `orElse` pure Nothing

-- | Parse a run of tokens that must have no line breaks.
chopRun ∷ StateT Int (Parser (Either (Token, Text) Room)) Text
chopRun =
  (close *> pure "") <|> fix \recurse → do
    (_, text) ← matchMaybe fromLeft
    room ← try (foresee (matchMaybe fromRight))
    modify (+ Text.length text)
    case room of
      Just Love → do
        _ ← matchMaybe fromRight
        fmap (text <>) recurse
      _ → pure text

-- | Parse appropriate white space between runs.
chopRoom ∷ StateT Int (Parser (Either (Token, Text) Room)) Text
chopRoom = do
  room ← lift do matchMaybe fromRight
  pad ← case room of
    Love → empty
    Hate → pure " "
    Indifference → pure ""
  text ← foresee chopRun
  lengthSoFar ← get
  let newLength = lengthSoFar + Text.length text + Text.length pad
  if newLength > 80
    then do
      put 0
      pure "\n"
    else do
      modify (+ Text.length pad)
      pure pad

cut80 ∷ StateT Int (Parser (Either (Token, Text) Room)) Text
cut80 = fmap Text.concat (parseStripes (fmap pure chopRun) (fmap pure chopRoom)) <* lift close

data RegularExpression input output where
  Pick ∷ (input → Bool) → RegularExpression input input
  Choice ∷ [RegularExpression input output] → RegularExpression input output
  Adjacent ∷ [RegularExpression input output] → RegularExpression input output
  Star ∷ RegularExpression input output → RegularExpression input output
  Match ∷ RegularExpression input output → RegularExpression input [output]
  Replace ∷ ([input] → [input]) → RegularExpression input input → RegularExpression input input

regularExpressionParser ∷ RegularExpression input output → Parser input [output]
regularExpressionParser = fix \recurse → \case
  Pick check → fmap pure do match check
  Choice regularExpressions → Applicative.asum (fmap recurse regularExpressions)
  Adjacent regularExpressions → foldr (Applicative.liftA2 (<>)) (pure []) (fmap recurse regularExpressions)
  Star regularExpression → fmap concat do Applicative.many (recurse regularExpression)
  Match regularExpression →
    let matcher = regularExpressionParser regularExpression
     in (fmap reverse . flip execStateT [] . fix) \parser → do
          maybeMatched ← lift do try matcher
          maybe (pure ()) (modify . (:)) maybeMatched
          _ ← match (const True)
          parser <|> close
  Replace replace regularExpression →
    let matcher = regularExpressionParser regularExpression
     in fix \parser → do
          maybeMatched ← try matcher
          let replaced = maybe [] replace maybeMatched
          fmap (replaced <>) do
            Applicative.liftA2 (:) (match (const True)) parser `orElse` (close *> pure [])

searchAndReplace ∷ ([token] → [token]) → RegularExpression token token → [token] → [token]
searchAndReplace replace regularExpression input =
  case runParser (regularExpressionParser (Replace replace regularExpression)) input of
    Just output → output
    Nothing → error "Something went wrong!"

repair ∷ [(Token, Text)] → [(Token, Text)]
repair tokens =
  let
    -- Sometimes GHC's lexer forgets to close the outermost curly brace.
    closingCurlyBraces = replicate ((length . filter ((== "{") . snd)) tokens - (length . filter ((== "}") . snd)) tokens) (ITccurly, "}")
    dropRedundantSemicolon = searchAndReplace (const [(ITccurly, "}")]) (Adjacent [Pick ((== ";") . snd), Pick ((== "}") . snd)])
    dropModuleHeader = searchAndReplace
      do const ([] ∷ [(Token, Text)])
      do Adjacent [Pick ((== "module") . snd), Pick ((== "Main") . snd), Pick ((== "where") . snd)]
   in
    (dropModuleHeader . dropRedundantSemicolon) (tokens <> closingCurlyBraces)

smallPrint ∷ (Text, Token) → Text
smallPrint (_, ITvocurly) = "{"
smallPrint (_, ITvccurly) = "}"
smallPrint (_, ITsemi) = ";"
smallPrint (_, (ITdocCommentNext _ _)) = mempty
smallPrint (_, (ITdocCommentPrev _ _)) = mempty
smallPrint (_, (ITdocCommentNamed _ _)) = mempty
smallPrint (_, (ITdocSection _ _ _)) = mempty
smallPrint (_, (ITdocOptions _ _)) = mempty
smallPrint (_, (ITlineComment _ _)) = mempty
smallPrint (_, (ITblockComment _ _)) = mempty
smallPrint (verbatim, _) = verbatim

render ∷ (Text, Token) → (Token, Text)
render this@(_, token) = (token, smallPrint this)

renameCurlies ∷ Text → Text
renameCurlies input =
  let
    (curlyConstructors, curlyVariables) =
      ( partition (isUpper . (!! 1))
          . fromMaybe (error "Cannot parse curlies!")
          . runParser parseCurlies
          . Text.unpack
      )
        input
    shortenings = Map.fromList (ListUtils.nubOrd curlyConstructors `zip` constructors <> ListUtils.nubOrd curlyVariables `zip` variables)
    shortenCurlies = (regularExpressionParser . Replace (shortenings Map.!)) curliesRegularExpression
   in
    (Text.pack . fromMaybe (error "Cannot replace curlies!") . runParser shortenCurlies . Text.unpack) input
 where
  parseCurlies = (regularExpressionParser . Match) curliesRegularExpression
  curliesRegularExpression = Adjacent [Pick (== '{'), Star (Pick isAllowedInIdentifier), Pick (== '}')]
  variables = inflate ['ऄ' .. 'ह']
  constructors = inflate ['Ա' .. 'Ֆ']
  inflate xs = fmap pure xs ++ [x : ys | ys ← inflate xs, x ← xs]

main ∷ IO ()
main = do
  Command {..} ← getRecord @IO @Command "Minify your Haskell files!"
  sourceCode ← Text.readFile source
  let uncurlifiedSourceCode = renameCurlies sourceCode
  let (hashLines, actualCode) = cut (all \line → (Text.head line == '#')) (Text.lines uncurlifiedSourceCode)
  let output = if target == "-" then Text.putStrLn else Text.writeFile target
  maybe exitFailure (output . (Text.unlines hashLines <>)) do
    pure (Text.unlines actualCode)
      >>= lexify
      >>= nonEmpty . repair . fmap render
      >>= runParser (evalStateT cut80 0) . flatten . tokensToStripes
