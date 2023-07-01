module CatRegex
( RegexToken
, RegexThread
, RegexBuilder(..)
, stringify
, exactly
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
, regexFromList
) where

import CatRegex.Internal.AST
import CatRegex.Internal.Blocks
import CatRegex.Internal.Stringify

regexFromList :: [RegexToken] -> RegexThread
regexFromList = fromList
