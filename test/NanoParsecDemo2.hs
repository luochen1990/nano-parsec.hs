module SimpleXMLParser where
import Control.Applicative
import Data.Char
import NanoParsec

--------------------------- demo for nano-parserc ---------------------------

data XML = MkXML String [(String, String)] [SubXML] deriving (Show, Eq)
data SubXML = Node XML | Text String deriving (Show, Eq)

pXML = do
  tagname <- single '<' *> pName
  attrs <- pAttrs <* single '>'
  body <- many pSubXML
  string "</" *> string tagname <* single '>'
  return $ MkXML tagname attrs body

pSubXML = Node <$> pXML <|> Text <$> pText

pText = some $ '<' <$ string "&le" <|> sat (/= '<')
pName = (:) <$> (sat isAlpha) <*> many (sat isAlphaNum)
pAttrs = many $ some (sat (==' ')) *> ((,) <$> pName <* single '=' <*> pValue)
pValue = some (sat isAlphaNum) <|> single '"' *> many (sat (/= '"')) <* single '"'

parse = runParser pXML

i ===> e = putStrLn (let o = parse i in if o /= e then "Fail: " ++ i ++ " ===> " ++ (show o) ++ "\n\tExpect: " ++ show e else "Ok.")
test = do
  "a" ===> Left "impossible to parse: \"a\""
  "<a></a>" ===> Right (MkXML "a" [] [])
  "<a></b>" ===> Left "impossible to parse: \"<a></b>\""
  "<a>hello, world</a>" ===> Right (MkXML "a" [] [Text "hello, world"])
  "<a>hello, &leworld></a>" ===> Right (MkXML "a" [] [Text "hello, <world>"])
  "<a height=1>hello, world</a>" ===> Right (MkXML "a" [("height", "1")] [Text "hello, world"])
  "<a height=1 title=\"click me\">hello, world</a>" ===> Right (MkXML "a" [("height", "1"), ("title", "click me")] [Text "hello, world"])

