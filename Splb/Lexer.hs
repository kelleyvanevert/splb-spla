{-# LANGUAGE MonadComprehensions #-}

{-
  Defines spl tokens,
   lexes spl programs
-}

module Splb.Lexer where
  -- splblex :: String -> [ [Token] ]

import ParserCombinators
import Control.Monad
-- import Debug.Trace

import Splb.Language


splblex :: String -> [[Token]]
splblex = map fst . parse lexer


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

lex_btype :: Parser String String
lex_btype = strings basic_types

lex_keyword :: Parser String String
lex_keyword = strings keywords

lex_symbol :: Parser String String
lex_symbol = strings symbols

lex_integer :: Parser String String
lex_integer = first $ many1 digit

lex_boolean :: Parser String String
lex_boolean = strings booleans

lex_unit :: Parser String String
lex_unit = string "#"

lex_operator :: Parser String String
lex_operator = strings operators



rm_trailing_junk :: Parser String a -> Parser String a
rm_trailing_junk p = [ a | a <- p, _ <- lex_junk ]

rm_leading_junk :: Parser String a -> Parser String a
rm_leading_junk p = [ a | _ <- lex_junk, a <- p ]

readb :: String -> Bool
readb "true" = True
readb "false" = False

lexer :: Parser String [Token]
lexer = successful $
        rm_leading_junk $
        first $
        many $
        list_or $
        map rm_trailing_junk
        [
          lex_integer  |> read |> Tk_Int,
          lex_boolean  |> readb |> Tk_Bool,
          lex_unit     |> (\_ -> Tk_Unit),
          lex_btype    |> Tk_Type,
          lex_symbol   |> Tk_Symbol,
          lex_operator |> Tk_Op,
          lex_keyword  |> Tk_Keyword,
          lex_ident    |> Tk_Ident
        ]

-- do { content <- readFile "facr.spl"; return (parse lexer content) }
