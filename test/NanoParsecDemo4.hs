module LambdaCalculus where

import Control.Applicative
import Data.Char
import NanoParsec

--------------------------- lambda calc demo  ---------------------------

type Ident = String
data Term =
      Lam Ident Term
    | App Term Term
    | Ref Ident
    deriving (Eq, Ord, Show)

pTerm = pLam

pLam = Lam <$> pIdent <* single '.' <* spaces <*> pLam <|> pApp
pApp = pAtom `assocl1` (pure App <* some space) <|> pAtom
pAtom = pRef <|> (single '(' *> pTerm <* single ')')
pRef = Ref <$> pIdent
pIdent = (:) <$> (sat isAlpha) <*> many (sat isAlphaNum)

parse = runParser pTerm

i ===> e = putStrLn (let o = parse i in if o /= e then "Fail:\t" ++ i ++ "\n\t====>   " ++ (show o) ++ "\n\tExpect: " ++ show e else "Ok.")
test = do
  "a" ===> Right (Ref "a")
  "a1" ===> Right (Ref "a1")
  "f x" ===> Right (App (Ref "f") (Ref "x"))
  "x. x" ===> Right (Lam "x" (Ref "x"))
  "f x y" ===> Right (App (App (Ref "f") (Ref "x")) (Ref "y"))
  "x. y. add x y" ===> Right (Lam "x" (Lam "y" (App (App (Ref "add") (Ref "x")) (Ref "y"))))

