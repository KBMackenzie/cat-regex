module Examples.Hex
( hexColor
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
