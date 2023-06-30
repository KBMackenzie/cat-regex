module Examples.UrlRegex
( urlRegex
) where

import CatRegex

-- Base for this regex (from: https://projects.lukehaas.me/regexhub/):
-- /^((https?|ftp|file):\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?$/

urlRegex :: String
urlRegex = stringify $
    startOfLine
    <.+> optionally (
        capture (
            (exactly "http" <.+> optionally (char 's'))
            .||+ exactly "file"
            .||+ exactly "ftp"
        )
        <.+> exactly "://"
    )
    <.+> capture (
        oneOrMore (digit .||+ char '.' .||+ inRange 'a' 'z')
    )
    <.+> char '.'
    <.+> capture (
        amountBetween 2 6 (inRange 'a' 'z' .||+ char '.')
    )
    <.+> capture (
        anyAmountOf (char '/' .||+ wordChar .||+ char ' ' .||+ char '.' .||+ char '-')
    )
    <.+> optionally (char '/')
    <.+> endOfLine
