module NanoParsec where
import Control.Applicative

------------------------ my nano parser combinator ------------------------

newtype Parser tok res = Parser { _p :: [tok] -> [(res, [tok])] }

runParser :: (Show t, Show a) => Parser t a -> [t] -> Either String a
runParser m s = case _p m s of [(r,[])] -> Right r; cs -> Left ("parse fail: " ++ show cs)

instance Alternative (Parser t) where
  empty = Parser (const [])
  p <|> p' = Parser (\s -> case _p p s of [] -> _p p' s; r -> r)

instance Functor (Parser t) where
  fmap f p = Parser (\s -> [(f a, b) | (a, b) <- _p p s])

instance Applicative (Parser t) where
  pure x = Parser (\s -> [(x, s)])
  pf <*> px = Parser (\s -> [(f x, s2) | (f, s1) <- _p pf s, (x, s2) <- _p px s1])

instance Monad (Parser t) where -- not required
  p >>= f = Parser (\s -> concatMap (\(a, s') -> _p (f a) s') (_p p s))

anySingle :: Parser t t
anySingle = Parser (\s -> case s of [] -> []; (c:cs) -> [(c, cs)])

eof :: Parser t ()
eof = Parser (\s -> case s of [] -> [((), [])]; _ -> [])

sat :: (t -> Bool) -> Parser t t -- `satisfy` in parsec
sat p = Parser (\s -> case s of [] -> []; (c:cs) -> [(c, cs) | p c])

single :: Eq t => t -> Parser t t
single x = sat (==x)

string :: Eq t => [t] -> Parser t [t]
string xs = foldr (liftA2 (:)) (pure []) (map single xs)

oneOf :: Eq t => (a -> Parser t b) -> [a] -> Parser t b
oneOf f = foldr (<|>) empty . map f

space :: Parser Char Char
space = sat (`elem` " \t\n\r")

spaces :: Parser Char String
spaces = many space

token :: Eq a => a -> Parser (a, b) b
token t = Parser (\s -> case s of [] -> []; ((x, y):s') -> [(y, s') | x == t])

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p op = liftA2 (:) p (many (op *> p))

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p op = liftA2 (:) p (many (op *> p)) <|> pure []

assocl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a -- `chainl1` in parsec
assocl1 p op = p <**> (foldr (flip (.)) id <$> many (flip <$> op <*> p))

assocl :: Parser t b -> Parser t (b -> a -> b) -> Parser t a -> Parser t b
assocl p0 op p = p0 <**> (foldr (flip (.)) id <$> many (flip <$> op <*> p))

assocr1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a -- `chainr1` in parsec
assocr1 p op = (foldr (.) id <$> many (p <**> op)) <*> p

assocr :: Parser t a -> Parser t (a -> b -> b) -> Parser t b -> Parser t b
assocr p op p0 = (foldr (.) id <$> many (p <**> op)) <*> p0

