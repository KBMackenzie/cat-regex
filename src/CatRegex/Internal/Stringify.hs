{-# LANGUAGE OverloadedStrings #-}

module CatRegex.Internal.Stringify
( stringify
) where

import CatRegex.Internal.AST
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.String (IsString(..))
import Data.Maybe (maybe)

stringify :: (RegexBuilder a, IsString b) => a -> b
stringify = fromString . Text.unpack . unrollThread . single

operator :: Operator -> Text.Text
operator Star = "*"
operator Plus = "+"
operator Question = "?"

genChar :: GenChar -> IsFlipped -> Text.Text
genChar gen flipped = (if flipped then Text.toUpper else id) $ case gen of
    Digit       -> "\\d"
    Space       -> "\\s"
    WordChar    -> "\\w"

shouldEscape :: Set.Set Char
shouldEscape = Set.fromList
    [ '*', '+', '?', '.', '(', ')', '\\', '/', '[', ']', '{', '}', ':', '^', '$', '|' ]

escapeChar :: Char -> Text.Text
escapeChar c = if Set.member c shouldEscape
    then Text.pack [ '\\', c ]
    else Text.singleton c

escapeString :: String -> Text.Text
escapeString = Text.concat . map escapeChar

maybeShow :: (Show a) => Maybe a -> Text.Text
{-# SPECIALIZE maybeShow :: Maybe Int -> Text.Text #-}
maybeShow = maybe "" (Text.pack . show)

unrollThread :: RegexThread -> Text.Text
unrollThread ThreadEnd = Text.empty
unrollThread (ThreadNode token ThreadEnd) = stringifyToken token
unrollThread (ThreadNode token next) = stringifyToken token `Text.append` unrollThread next

unrollForOp :: RegexThread -> Text.Text
unrollForOp ThreadEnd = Text.empty -- Shouldn't happen.
unrollForOp (ThreadNode token ThreadEnd) = stringify token
unrollForOp other = nonCaptureGroup (unrollThread other)

between :: Text.Text -> Text.Text -> Text.Text -> Text.Text
between a c b = Text.concat [ a, b, c ]

captureGroup :: Text.Text -> Text.Text
captureGroup = between "(" ")"

atomicGroup :: Text.Text -> Text.Text
atomicGroup = between "(?>" ")"

nonCaptureGroup :: Text.Text -> Text.Text
nonCaptureGroup = between "(?:" ")"

namedCaptureGroup :: String -> Text.Text -> Text.Text
namedCaptureGroup name = between start ")"
    where start = Text.concat [ "(?<", Text.pack name, ">" ]

range :: Text.Text -> Text.Text
range = between "{" "}"

characterClass :: Text.Text -> Text.Text
characterClass = between "[" "]"

stringifyToken :: RegexToken -> Text.Text
stringifyToken (Exactly str) = escapeString str
stringifyToken (Single char) = escapeChar char
stringifyToken (Operation op xs) = unrollForOp xs `Text.append`  operator op
stringifyToken (CharGroup flipped xs) = characterClass $ if flipped
    then "^" `Text.append` Text.pack xs
    else Text.pack xs
stringifyToken (CharRange flipped start end) = characterClass $ if flipped
    then "^" `Text.append` Text.concat [escapeChar start, "-", escapeChar end]
    else Text.pack [start, '-', end]
stringifyToken (CharRanges flipped xs) = let f (a, b) = [ escapeChar a, "-", escapeChar b ]
    in characterClass $ if flipped
        then "^" `Text.append` (Text.concat . concatMap f) xs
        else (Text.concat . concatMap f) xs
stringifyToken (GenericChar gen flipped) = genChar gen flipped
stringifyToken (CaptureGroup xs) = captureGroup (unrollThread xs)
stringifyToken (AtomicGroup xs) = atomicGroup (unrollThread xs)
stringifyToken (NonCaptureGroup xs) = nonCaptureGroup (unrollThread xs)
stringifyToken (NamedCapture name xs) = namedCaptureGroup name (unrollThread xs)
stringifyToken (Count n xs) = unrollForOp xs `Text.append` (range . Text.pack . show) n
stringifyToken (CountRange a b xs) = unrollForOp xs `Text.append`
    (range . Text.concat) [ maybeShow a, ",", maybeShow b ]
stringifyToken (Alternative a b) = (nonCaptureGroup . Text.concat) [ unrollThread a, "|", unrollThread b ]
stringifyToken AnyChar = "."
stringifyToken (Unescaped str) = Text.pack str
stringifyToken (SingleUnescaped c) = Text.singleton c
stringifyToken LineStart = "^"
stringifyToken LineEnd = "$"
