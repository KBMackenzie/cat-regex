module Examples.Foo
(
) where

import CatRegex

example :: String
example = stringify $
    startOfLine
    <.+> exactly "hello"
    <.+> anyAmountOf whitespace
    <.+> exactly "world" .||+ exactly "wORLD" .||+ exactly "WORLDDD >*<"
    <.+> oneOrMore (char '!')
    <.+> endOfLine

example' :: String
example' = stringify $ regexFromList
    [ startOfLine
    , exactly "hello"
    , anyAmountOf whitespace
    , exactly "world" .||+ exactly "wORLD" .||+ exactly "WORLDDD >*<"
    , oneOrMore (char '!')
    , endOfLine ]
