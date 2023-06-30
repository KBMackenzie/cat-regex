module Examples.HTML
( htmlTag
, htmlTag'
, imageTagSrc
) where

import CatRegex

-- Example: <(?>/?)(?!p).+?>

htmlTag :: String
htmlTag = stringify $
    char '<'
    <.+> optionally (char '/')
    <.+> oneOrMore anyChar
    <.+> char '>'

htmlTag' :: String -- With captures.
htmlTag' = stringify $
    char '<'
    <.+> capture (optionally (char '/'))
    <.+> capture (oneOrMore anyChar)
    <.+> char '>'

-- /^<\s*img[^>]+src\s*=\s*(["'])(.*?)\1[^>]*>$/

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
