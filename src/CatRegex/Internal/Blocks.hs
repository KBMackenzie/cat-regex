module CatRegex.Internal.Blocks
( exactly
, char
, optionally
, oneOrMore
, anyAmountOf
, capture
, oneOf
, notOneOf
, inRange
, notInRange
, inRanges
, notInRanges
, amountOf
, amountBetween
, minimumOf
, maximumOf
, (.||+)
, anyChar
, atomic
, groupUp
, namedCapture
, digit
, notDigit
, whitespace
, notWhitespace
, wordChar
, notWordChar
, startOfLine
, endOfLine
, char'
, exactly'
) where

import CatRegex.Internal.AST
import qualified Data.Text as Text

exactly :: String -> RegexToken
exactly = Exactly

char :: Char -> RegexToken
char = Single

optionally :: (RegexBuilder a) => a -> RegexToken
optionally = Operation Question . single

oneOrMore :: (RegexBuilder a) => a -> RegexToken
oneOrMore = Operation Plus . single

anyAmountOf :: (RegexBuilder a) => a -> RegexToken
anyAmountOf = Operation Star . single

capture :: (RegexBuilder a) => a -> RegexToken
capture = CaptureGroup . single

oneOf :: [Char] -> RegexToken
oneOf = CharGroup False

notOneOf :: [Char] -> RegexToken
notOneOf = CharGroup True

inRange :: Char -> Char -> RegexToken
inRange = CharRange False

notInRange :: Char -> Char -> RegexToken
notInRange = CharRange True

inRanges :: [(Char, Char)] -> RegexToken
inRanges = CharRanges False

notInRanges :: [(Char, Char)] -> RegexToken
notInRanges = CharRanges True

amountOf :: (RegexBuilder a) => Int -> a -> RegexToken
amountOf a = Count a . single

amountBetween :: (RegexBuilder a) => Int -> Int -> a -> RegexToken
amountBetween a b = CountRange (Just a) (Just b) . single

minimumOf :: (RegexBuilder a) => Int -> a -> RegexToken
minimumOf a = CountRange (Just a) Nothing . single

maximumOf :: (RegexBuilder a) => Int -> a -> RegexToken
maximumOf b = CountRange Nothing (Just b) . single

infixr 6 .||+
(.||+) :: (RegexBuilder a, RegexBuilder b) => a -> b -> RegexToken
(.||+) a b = Alternative (single a) (single b)
-- Note to self: always foldr when evaluating this.

anyChar :: RegexToken
anyChar = AnyChar

atomic :: (RegexBuilder a) => a -> RegexToken
atomic = AtomicGroup . single

groupUp :: (RegexBuilder a) => a -> RegexToken
groupUp = NonCaptureGroup . single

namedCapture :: (RegexBuilder a) => String -> a -> RegexToken
namedCapture name = NamedCapture name . single

-- Generic character classes:

digit :: RegexToken
digit = GenericChar Digit False

notDigit :: RegexToken
notDigit = GenericChar Digit True

whitespace :: RegexToken
whitespace = GenericChar Space False

notWhitespace :: RegexToken
notWhitespace = GenericChar Space True

wordChar :: RegexToken
wordChar = GenericChar WordChar False

notWordChar :: RegexToken
notWordChar = GenericChar WordChar True


-- Anchors

startOfLine :: RegexToken
startOfLine = LineStart

endOfLine :: RegexToken
endOfLine = LineEnd


-- Unescaped string variants:

char' :: Char -> RegexToken
char' = SingleUnescaped

exactly' :: String -> RegexToken
exactly' = Unescaped
