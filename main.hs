main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Hello title"
    (append_ (h1_ "Hello, world!") (p_ "Let's learn about Haskell!"))

html_ :: Title -> Structure -> Html
html_ title (Structure body) =
  Html
    ( getStructureString
        ( append_
            ( el
                "html"
                ( getStructureString
                    ( head_
                        (getStructureString (title_ title))
                    )
                )
            )
            (body_ body)
        )
    )

body_ :: String -> Structure
body_ =
  el "body"

head_ :: String -> Structure
head_ =
  el "head"

title_ :: String -> Structure
title_ =
  el "title"

p_ :: String -> Structure
p_ =
  el "p"

h1_ :: String -> Structure
h1_ =
  el "h1"

el :: String -> String -> Structure
el tag content =
  Structure ("<" <> tag <> ">" <> content <> "</" <> tag <> ">")

newtype Html = Html String

newtype Structure = Structure String

type Title = String

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

render :: Html -> String
render html =
  case html of
    Html s -> s

getStructureString :: Structure -> String
getStructureString (Structure s) = s
