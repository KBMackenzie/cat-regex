module CatRegex.Internal.AST
( RegexToken(..)
, GenChar(..)
, Operator(..)
, RegexThread(..)
, RegexBuilder(..)
, IsFlipped
, unroll
, asList
, fromList
) where

import qualified Data.Text as Text

type IsFlipped = Bool

data RegexToken =
      Exactly String
    | Single Char
    | CharRange IsFlipped Char Char
    | CharGroup IsFlipped [Char]
    | CharRanges IsFlipped [(Char, Char)]
    | GenericChar GenChar IsFlipped 
    | Operation Operator RegexThread
    | Alternative RegexThread RegexThread
    | CaptureGroup RegexThread
    | NamedCapture String RegexThread
    | AtomicGroup RegexThread
    | NonCaptureGroup RegexThread
    | Count Int RegexThread
    | CountRange (Maybe Int) (Maybe Int) RegexThread
    | AnyChar
    | Unescaped String
    | SingleUnescaped Char
    | LineStart | LineEnd
    deriving (Eq, Show)

data GenChar =
      Digit
    | Space
    | WordChar
    deriving (Eq, Show, Enum)

data Operator =
      Star
    | Plus
    | Question
    deriving (Eq, Show, Enum)

data RegexThread =
      ThreadNode RegexToken RegexThread
    | ThreadEnd
    deriving (Eq, Show)

class RegexBuilder a where
    infixr 5 <.+> -- Same precedence as ':'.
    (<.+>) :: RegexToken -> a -> RegexThread
    single :: a -> RegexThread

instance RegexBuilder RegexThread where
    (<.+>) = ThreadNode
    single = id

instance RegexBuilder RegexToken where
    a <.+> b = ThreadNode a (ThreadNode b ThreadEnd)
    single   = flip ThreadNode ThreadEnd

unroll :: RegexThread -> [RegexToken]
unroll (ThreadNode token next) = token : unroll next
unroll ThreadEnd               = []

asList :: (RegexBuilder a) => a -> [RegexToken]
asList = unroll . single

fromList :: [RegexToken] -> RegexThread
fromList = foldr (<.+>) ThreadEnd
