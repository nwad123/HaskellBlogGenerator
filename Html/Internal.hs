module Html.Internal where

-- * Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure body) =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" body
        )
    )

body_ :: String -> Structure
body_ =
  els "body"

head_ :: String -> Structure
head_ =
  els "head"

title_ :: String -> Structure
title_ =
  els "title"

p_ :: String -> Structure
p_ =
  els "p"

h1_ :: String -> Structure
h1_ =
  els "h1"

ul_ :: [Structure] -> Structure
ul_ =
  let listItem i = el "li" (getStructureString i)
   in Structure . (el "ul") . concat . map listItem

ol_ :: [Structure] -> Structure
ol_ =
  let listItem i = el "li" (getStructureString i)
   in Structure . (el "ol") . concat . map listItem

code_ :: String -> Structure 
code_ =
  els "pre"

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html s -> s

els :: String -> String -> Structure
els tag =
  Structure . (el tag) . escape

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure s) = s

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concat . map escapeChar
