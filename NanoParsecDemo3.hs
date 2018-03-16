module MoleculeToAtoms where

import Data.List (sort)
import NanoParsec
import Control.Applicative
import Data.Char
import qualified Data.Map as M

type Token = (String, String)

tokenize :: String -> Either String [Token]
tokenize = runParser (some tok) where
  tok = (,) "Atom" <$> ((:) <$> satisfy isUpper <*> many (satisfy isLower))
    <|> (,) "Numb" <$> some (satisfy isDigit)
    <|> (,) "Brac" <$> pure <$> (satisfy (`elem` "(){}[]"))

pCount :: Parser Token (M.Map String Int)
pCount = M.unionsWith (+) <$> some pTaged
pTaged = (\a k -> M.map (*k) a) <$> pPrim <*> (maybe 1 id <$> optional (read <$> token "Numb"))
pPrim = (M.singleton <$> token "Atom" <*> pure 1) <|> pBrac
pBrac = single ("Brac", "(") *> pCount <* single ("Brac", ")")
    <|> single ("Brac", "{") *> pCount <* single ("Brac", "}")
    <|> single ("Brac", "[") *> pCount <* single ("Brac", "]")

parse :: String -> Either String [(String,Int)]
parse formula = M.toList <$> ((tokenize formula) >>= runParser pCount)

-- test --

test :: IO ()
test = do
  "H" ===> Right [("H",1)] --"hydrogen"
  "O2" ===> Right [("O",2)] --"oxygen"
  "H2O" ===> Right [("H",2),("O",1)] --"water"
  "Mg(OH)2" ===> Right [("Mg",1),("O",2),("H",2)] --"magnesium hydroxide"
  "K4[ON(SO3)2]2" ===> Right [("K",4),("O",14),("N",2),("S",4)] --"Fremy's salt"
  "pie" ===> Left "impossible to parse: \"pie\"" --"Not a valid molecule"
  "Mg(OH" ===> Left "not finised: (fromList [(\"Mg\",1)],[(\"Brac\",\"(\"),(\"Atom\",\"O\"),(\"Atom\",\"H\")])" --"Mismatched parenthesis"
  "Mg(OH}2" ===> Left "not finised: (fromList [(\"Mg\",1)],[(\"Brac\",\"(\"),(\"Atom\",\"O\"),(\"Atom\",\"H\"),(\"Brac\",\"}\"),(\"Numb\",\"2\")])" --"Mismatched parenthesis"

i ===> e = putStrLn (let o = sort <$> parse i in if o /= fmap sort e then "Fail: " ++ i ++ " ===> " ++ (show o) ++ "\n\tExpect: " ++ show e else "Ok.")
