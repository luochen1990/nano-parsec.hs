module NanoParsecDemo where
import Control.Applicative
import NanoParsec

--------------------------- demo for nano-parserc ---------------------------

data BoolExp = T | F | Not BoolExp | And BoolExp BoolExp | Or BoolExp BoolExp
  deriving (Show, Eq)

-- priority: T|F|() > Not > And (-- right assoc) > Or (-- right assoc)
pExpr = pOr <|> pAnd <|> pNot <|> pAtom

pOr = Or <$> pAnd <* token '|' <*> pOr <|> pAnd                     -- binary
pAnd = (uncurry And) <$> (pNot, pAnd) `pairBy` token '&' <|> pNot   --
pNot = Not <$> (token '!' *> pAtom) <|> pAtom                       -- unary
pAtom = pLit <|> (token '(' *> pExpr <* token ')')                  --
pLit = T <$ token 't' <|> pure F <* token 'f'                       -- nullary

parse = runParser pExpr

i ===> e = putStrLn (let o = parse i in if o /= e then "Fail: " ++ i ++ " ===> " ++ (show o) ++ "\n\tExpect: " ++ show e else "Ok.")
test = do
  "a" ===> Left "impossible to parse: \"a\""
  "t" ===> Right T
  "tt" ===> Left "not finised: (T,\"t\")"
  "t&f" ===> Right (And T F)
  "!t|f" ===> Right (Or (Not T) F)
  "t&f|f&t|t&f" ===> Right (Or (And T F) (Or (And F T) (And T F)))
  "t|f&f|t&t|f" ===> Right (Or T (Or (And F F) (Or (And T T) F)))
