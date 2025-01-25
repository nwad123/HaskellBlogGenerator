import Html

main :: IO ()
main = putStrLn (render myHtml)

myHtml :: Html
myHtml =
  html_
    "Hello title"
    (append_ (h1_ "Hello, world!") (p_ "Let's learn about Haskell!"))
