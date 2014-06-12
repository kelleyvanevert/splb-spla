{-# LANGUAGE MonadComprehensions #-}

{-
  A small and generic parser combinator library

  :set -XMonadComprehensions
-}

module ParserCombinators where


import Control.Monad
-- import Debug.Trace

data Parser s a = Parser (s -> [(a, s)])

parse :: Parser s a -> s -> [(a, s)]
parse (Parser p) i = p i

instance Monad (Parser s) where
  return a  = Parser $ \cs -> [(a, cs)]
  p >>= f   = Parser $ \cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs]
  fail _    = Parser $ \cs -> []

instance MonadPlus (Parser s) where
  mzero     = Parser $ \cs -> []
  mplus p q = Parser $ \cs -> parse p cs ++ parse q cs


--- generic parser combinators

(|||) :: Parser s a -> Parser s a -> Parser s a
(|||) = mplus

list_or :: [Parser s a] -> Parser s a
list_or ps = foldl mplus mzero ps

(|>) :: Parser s a -> (a -> b) -> Parser s b
(|>) p f = [ f a | a <- p ]

first :: Parser s a -> Parser s a
first p = Parser $
  \i -> case parse p i of
    [] -> []
    (x:xs) -> [x]

(||||) :: Parser s a -> Parser s a -> Parser s a
(||||) p q = Parser $
  \cs -> case parse (p `mplus` q) cs of
    [] -> []
    (x:xs) -> [x]

(<^^) :: Parser s a -> Parser s b -> Parser s a
(<^^) p q = [ x | x <- p, _ <- q ]

(^^>) :: Parser s a -> Parser s b -> Parser s b
(^^>) p q = [ y | _ <- p, y <- q ]

many :: Parser s a -> Parser s [a]
many p = many1 p ||| return []

many1 :: Parser s a -> Parser s [a]
many1 p = [ a : as | a <- p, as <- many p ]

sepby :: Parser s a -> Parser s b -> Parser s [a]
p `sepby` sep = (p `sepby1` sep) ||| return []

sepby1 :: Parser s a -> Parser s b -> Parser s [a]
p `sepby1` sep = [ a : as | a <- p, as <- many (do { sep; p }) ]

possibly :: Parser s a -> Parser s [a]
possibly p = [ [a] | a <- p ] |||| return []

chainl1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 p op = p >>= rest
  where
    rest x =  do { f <- op; y <- p; rest (f x y) } ||| (return x)

chainr1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr1 p op = p >>= \x -> [ f x y | f <- op, y <- p `chainr1` op ] ||| (return x)



--- list input parsers

has_input :: Parser [a] Bool
has_input = Parser $
  \cs -> case cs of
    [] -> [(False, cs)]
    _ -> [(True, cs)]

next :: Parser [a] a
next = Parser $
  \cs -> case cs of
    [] -> []
    (c:cs) -> [(c, cs)]

sat :: (a -> Bool) -> Parser [a] a
sat p = do
  c <- next
  if p c then return c else mzero

element :: Eq a => a -> Parser [a] a
element c = sat (c ==)

successful :: Parser [s] a -> Parser [s] a
successful p = [ a | a <- p, False <- has_input ]


--- string input parsers

lower :: Parser String Char
lower = sat (\c -> c >= 'a' && c <= 'z')

upper :: Parser String Char
upper = sat (\c -> c >= 'A' && c <= 'Z')

alpha :: Parser String Char
alpha = lower ||| upper

digit :: Parser String Char
digit = sat (\c -> c >= '0' && c <= '9')

alphanum :: Parser String Char
alphanum = alpha ||| digit

string :: String -> Parser String String
string ""     = return ""
string (c:cs) = [ c : cs | _ <- element c, _ <- string cs ]

strings :: [String] -> Parser String String
strings ss = list_or (map string ss)