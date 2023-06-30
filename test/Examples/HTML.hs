module Examples.HTML
( htmlTag
, htmlTag'
, imageTagSrc
) where

import CatRegex

-- Output: <\/?.+>
htmlTag :: String
htmlTag = stringify $
    char '<'
    <.+> optionally (char '/')
    <.+> oneOrMore anyChar
    <.+> char '>'

-- Output: <(\/?)(.+)>
htmlTag' :: String
htmlTag' = stringify $
    char '<'
    <.+> capture (optionally $ char '/')
    <.+> capture (oneOrMore anyChar)
    <.+> char '>'

-- Output: <\s*img[^>]+src\s*["'](.*)["'][^>]*>
imageTagSrc :: String
imageTagSrc = stringify $
    char '<'
    <.+> anyAmountOf whitespace
    <.+> exactly "img"
    <.+> oneOrMore (notOneOf ['>'])
    <.+> exactly "src"
    <.+> anyAmountOf whitespace
    <.+> oneOf [ '"', '\'' ]
    <.+> capture (anyAmountOf anyChar)
    <.+> oneOf [ '"', '\'' ]
    <.+> anyAmountOf (notOneOf ['>'])
    <.+> char '>'
