module NanoParsec where
import Control.Applicative

------------------------- my nano parser combinator -------------------------

newtype Parser tok val = Parser { _parse :: [tok] -> [(val, [tok])] }

runParser :: (Show t, Show a) => Parser t a -> [t] -> Either String a
runParser m s = case _parse m s of
  [(res, [])] -> Right res
  [c@(_, _)] -> Left ("not finised: " ++ show c)
  [] -> Left ("impossible to parse: " ++ show s)
  cs -> Left ("ambiguous: " ++ show cs)

item :: Parser t t
item = Parser (\s -> case s of [] -> []; (c:cs) -> [(c, cs)])

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser (\s -> case s of [] -> []; (c:cs) -> [(c, cs) | p c])

token :: Eq t => t -> Parser t t
token x = satisfy (==x)

tokens :: Eq t => [t] -> Parser t [t]
tokens xs = foldr (liftA2 (:)) (pure []) (map token xs)

sepBy :: Parser t a -> Parser t b -> Parser t [a]
sepBy p s = sepBy1 p s <|> pure []

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p s = liftA2 (:) p (many (s *> p))

pair :: (Parser t a, Parser t b) -> Parser t (a, b)
pair (p, q) = liftA2 (,) p q

pairBy :: (Parser t a, Parser t b) -> Parser t s -> Parser t (a, b)
pairBy (p, q) s = liftA2 (,) (p <* s) q

instance Alternative (Parser t) where
  empty = Parser (\cs -> [])
  (<|>) p q = Parser $ \s -> case _parse p s of [] -> _parse q s; r -> r

instance Functor (Parser t) where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative (Parser t) where
  pure x = Parser (\s -> [(x, s)])
  p <*> q = Parser (\s -> [(f a, s2) | (f, s1) <- _parse p s, (a, s2) <- _parse q s1])

--instance Monad (Parser t) where --NOTE: not necessary most of the time
--  return = pure
--  (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> _parse (f a) s') $ _parse p s

--instance MonadPlus (Parser t) where
--  mzero = Parser (\cs -> [])
--  mplus p q = Parser (\s -> _parse p s ++ _parse q s)

