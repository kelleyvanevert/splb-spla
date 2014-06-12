{-# LANGUAGE MonadComprehensions #-}

{-
  Defines spl tokens,
   lexes spl programs
-}

module Spla.Lexer where
  -- splalex :: String -> [ [Token] ]

import ParserCombinators
import Control.Monad
-- import Debug.Trace

import Spla.Language


splalex :: String -> [[Token]]
splalex = map fst . parse lexer


lex_single_line_comment :: Parser String ()
lex_single_line_comment = do
  string "//"
  first $ many (sat (/= '\n'))
  return ()

consume_multiline_comment :: Int -> Parser String ()
consume_multiline_comment n = do
  c <- next
  case c of
    '*' -> do
      c <- next
      case c of
        '/' -> if n == 0 then return () else consume_multiline_comment (n - 1)
        _ -> consume_multiline_comment n
    '/' -> do
      c <- next
      case c of
        '*' -> consume_multiline_comment (n + 1)
        _ -> consume_multiline_comment n
    _ -> consume_multiline_comment n

lex_multiline_comment :: Parser String ()
lex_multiline_comment = [ () | _ <- string "/*", _ <- consume_multiline_comment 0 ]

lex_whitespace :: Parser String ()
lex_whitespace = do
  list_or (map element whitespace)
  return ()

lex_junk :: Parser String ()
lex_junk = do
  first $ many $ list_or [
    lex_single_line_comment,
    lex_multiline_comment,
    lex_whitespace]
  return ()




lex_ident :: Parser String String
lex_ident = do
  c <- alpha
  cs <- first $ many (element '_' ||| element '\'' ||| alphanum)
  if (c:cs) `elem` keywords
    then mzero
    else return (c:cs)

lex_keyword :: Parser String String
lex_keyword = strings keywords

lex_char :: Parser String Char
lex_char = (element '\'') ^^> next <^^ (element '\'')

lex_string :: Parser String String
lex_string = do
  element '"'
  s <- first $ many (sat (/= '"'))
  element '"'
  return s

lex_integer :: Parser String String
lex_integer = first $ many1 digit

lex_boolean :: Parser String String
lex_boolean = strings booleans

lex_operator :: Parser String String
lex_operator = strings operators



rm_trailing_junk :: Parser String a -> Parser String a
rm_trailing_junk p = [ a | a <- p, _ <- lex_junk ]

rm_leading_junk :: Parser String a -> Parser String a
rm_leading_junk p = [ a | _ <- lex_junk, a <- p ]

readb :: String -> Bool
readb "true" = True
readb "false" = False

delete :: (Eq a) => a -> [a] -> [a]
delete e (x:xs) = if e == x then delete e xs else x:(delete e xs)
delete e [] = []

lexer :: Parser String [Token]
lexer = successful $
        rm_leading_junk $
        first $
        many $
        list_or $
        map rm_trailing_junk
        [
          lex_char                       |> Tk_Char,
          lex_string                     |> Tk_String,
          lex_integer                    |> read |> Tk_Int,
          lex_boolean                    |> readb |> Tk_Bool,
          (strings $ delete "=" symbols) |> Tk_Symbol,
          lex_operator                   |> Tk_Op,
          (string "=")                   |> Tk_Symbol,
          lex_ident                      |> Tk_Ident,
          lex_keyword                    |> Tk_Keyword
        ]

-- do { content <- readFile "facr.spl"; return (parse lexer content) }
