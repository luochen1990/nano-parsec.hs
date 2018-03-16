module NanoParsec where
import Control.Applicative

------------------------ my nano parser combinator ------------------------

newtype Parser tok res = Parser { prob :: [tok] -> [(res, [tok])] }

runParser :: (Show t, Show a) => Parser t a -> [t] -> Either String a
runParser m s = case prob m s of
  [(res, [])] -> Right res
  [c@(_, _)] -> Left ("not finised: " ++ show c)
  [] -> Left ("impossible to parse: " ++ show s)
  cs -> Left ("ambiguous: " ++ show cs)

item :: Parser t t
item = Parser (\s -> case s of [] -> []; (c:cs) -> [(c, cs)])

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser (\s -> case s of [] -> []; (c:cs) -> [(c, cs) | p c])

single :: Eq t => t -> Parser t t
single x = satisfy (==x)

string :: Eq t => [t] -> Parser t [t]
string xs = foldr (liftA2 (:)) (pure []) (map single xs)

token :: Eq a => a -> Parser (a, b) b
token tag = Parser (\s -> case s of [] -> []; ((t,r):s') -> [(r, s') | t == tag])

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p s = sepBy1 p s <|> pure []

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p s = liftA2 (:) p (many (s *> p))

space :: Parser Char Char
space = satisfy (`elem` " \t\n\r")

spaces :: Parser Char String
spaces = many space

singles :: Eq t => [t] -> Parser t t
singles = foldr (<|>) empty . map single

strings :: Eq t => [[t]] -> Parser t [t]
strings = foldr (<|>) empty . map string

eof :: Parser t ()
eof = Parser (\s -> case s of [] -> [((), [])]; _ -> [])

assocl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
assocl1 p op = p <**> (foldr (.) id <$> many (flip <$> op <*> p))

assocl :: Parser t b -> Parser t (b -> a -> b) -> Parser t a -> Parser t b
assocl p0 op p = p0 <**> (foldr (.) id <$> many (flip <$> op <*> p))

instance Alternative (Parser t) where
  empty = Parser (\cs -> [])
  (<|>) p q = Parser $ \s -> case prob p s of [] -> prob q s; r -> r

instance Functor (Parser t) where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative (Parser t) where
  pure x = Parser (\s -> [(x, s)])
  p <*> q = Parser (\s -> [(f a, s2) | (f, s1) <- prob p s, (a, s2) <- prob q s1])

instance Monad (Parser t) where --NOTE: not necessary most of the time
  return = pure
  (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> prob (f a) s') $ prob p s

--instance MonadPlus (Parser t) where
--  mzero = Parser (\cs -> [])
--  mplus p q = Parser (\s -> prob p s ++ prob q s)

