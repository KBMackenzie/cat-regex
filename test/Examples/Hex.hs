module Examples.Hex
( hexColor
, hexColor'
) where

import CatRegex

-- Valid hexadecimal digits.
hexDigits :: RegexToken
hexDigits = digit .||+ inRange 'a' 'f' .||+ inRange 'A' 'F'

-- Regex string using the helper above.
hexColor :: String
hexColor = stringify $
    startOfLine
    <.+> optionally (char '#')
    <.+> capture (
        amountOf 6 hexDigits
        .||+ amountOf 3 hexDigits
    )
    <.+> endOfLine

-- Output: ^#?((?:(?:\d|(?:[a-f]|[A-F])){6}|(?:\d|(?:[a-f]|[A-F])){3}))$

hexDigits' :: RegexToken
hexDigits' = inRanges [('a', 'f'), ('A', 'F'), ('0', '9')]

hexColor' :: String
hexColor' = stringify $
    startOfLine
    <.+> optionally (char '#')
    <.+> capture (
        amountOf 6 hexDigits'
        .||+ amountOf 3 hexDigits'
    )
    <.+> endOfLine

-- Output: ^#?((?:[a-fA-F0-9]{6}|[a-fA-F0-9]{3}))$
