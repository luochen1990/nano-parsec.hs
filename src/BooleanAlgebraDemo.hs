module BooleanAlgebraDemo where
import Control.Applicative
import NanoParsec

--------------------------- demo for nano-parserc ---------------------------

data BoolExp = T | F | Not BoolExp | And BoolExp BoolExp | Or BoolExp BoolExp
  deriving (Show, Eq)

-- priority: T|F|() > Not > And (-- right assoc) > Or (-- right assoc)
pExpr = pOr <|> pAnd <|> pNot <|> pAtom

pOr = Or <$> pAnd <* single '|' <*> pOr <|> pAnd              -- binary
pAnd = And <$> pNot <* single '&' <*> pAnd <|> pNot
pNot = Not <$> (single '!' *> pAtom) <|> pAtom                -- unary
pAtom = pLit <|> (single '(' *> pExpr <* single ')')
pLit = T <$ single 't' <|> pure F <* single 'f'               -- nullary

parse = runParser pExpr

i ===> e = putStrLn (let o = parse i in if o /= e then "Fail: " ++ i ++ " ===> " ++ (show o) ++ "\n\tExpect: " ++ show e else "Ok.")
test = do
  "a" ===> Left "parse failed! rest paths: []"
  "t" ===> Right T
  "tt" ===> Left "parse failed! rest paths: [(T,\"t\")]"
  "t&f" ===> Right (And T F)
  "!t|f" ===> Right (Or (Not T) F)
  "t&f|f&t|t&f" ===> Right (Or (And T F) (Or (And F T) (And T F)))
  "t|f&f|t&t|f" ===> Right (Or T (Or (And F F) (Or (And T T) F)))

