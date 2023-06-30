# cat-regex
CatRegex is a tiny Regex-builder library written in pure Haskell. You can use it to build PCRE-compliant Regex strings in a simple, readable way with pure Haskell code.

It's heavily inspired by the [magic-regexp](https://regexp.dev/) library for Javascript/Typescript.

This library builds an abstract syntax tree behind the scenes and then efficiently turns it into a PCRE-compliant Regex string. It's lazy by default.

The *'stringify'* function is polymorphic in its return type, and you can cast it to any of Haskell's string types. It uses the IsString typeclass, the same typeclass used by the OverloadedStrings language extension.

A few examples:

1. Regex for extracting the src string from an HTML \<img\> tag:

```haskell
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

    -- Output: <\s*img[^>]+src\s*["'](.*)["'][^>]*>
```
