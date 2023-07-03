module Examples.HTML
( htmlTag
, htmlTag'
, imageTagSrc
, imageTagSrc'
) where

import CatRegex

htmlTag :: String
htmlTag = stringify $
    char '<'
    <.+> optionally (char '/')
    <.+> oneOrMore anyChar
    <.+> char '>'
    -- Output: <\/?.+>

htmlTag' :: String
htmlTag' = stringify $
    char '<'
    <.+> capture (optionally $ char '/')
    <.+> capture (oneOrMore anyChar)
    <.+> char '>'
    -- Output: <(\/?)(.+)>

imageTagSrc :: String
imageTagSrc = stringify $
    char '<'
    <.+> anyAmountOf whitespace
    <.+> exactly "img"
    <.+> capture (anyAmountOf anyChar)
    <.+> char '>'
    -- Output: <\s*img(.*)>

imageTagSrc' :: String
imageTagSrc' = stringify $ regexFromList
    [ char '<'
    , anyAmountOf whitespace
    , exactly "img"
    , capture (anyAmountOf anyChar)
    , char '>' ]
    -- Output: <\s*img(.*)>
