{-# LANGUAGE MonadComprehensions #-}

{-
  Parses spl token lists into spl ASTs

  Expression parsing resources:
    - http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.html
    - http://villane.wordpress.com/2012/01/16/mixfix-operators-parser-combinators-part-1/
      http://villane.wordpress.com/2012/01/17/mixfix-operators-parser-combinators-part-2/
      http://villane.wordpress.com/2012/01/21/mixfix-operators-parser-combinators-bonus-part-2a/
-}

module Spla.Parser where
  -- splaparse :: [Token] -> Program

import Data.Char
import Data.List
import Control.Monad
import qualified Data.Map as Map
-- import Debug.Trace

import ParserCombinators

import Spla.Language



{-
  Common
  ======
-}

parse_identifier :: Parser [Token] String
parse_identifier = [ id | Tk_Ident id <- next ]

parse_match :: Parser [Token] a -> Parser [Token] (Match a)
parse_match parse_to = [ Match e rules | Tk_Keyword "match" <- next,
                                         e <- parse_expr "all",
                                         Tk_Symbol "{" <- next,
                                         rules <- (possibly (element $ Tk_Symbol "|"))
                                          ^^> (parse_match_rule parse_to) `sepby` (element $ Tk_Symbol "|"),
                                         Tk_Symbol "}" <- next ]

parse_match_rule :: Parser [Token] a -> Parser [Token] (MatchRule a)
parse_match_rule parse_to = [ MatchRule e to | e <- parse_expr "match",
                                               Tk_Symbol "->" <- next,
                                               to <- parse_to ]



{-
  Parsing program code
  ====================
-}

splaparse :: Parser [Token] Program
splaparse = parse_program

parse_program :: Parser [Token] Program
parse_program =
  successful $
  [ Program typeAliasDecls stmts | typeAliasDecls <- many $ parse_type_decl,
                                       stmts <- many $ parse_statement ]

parse_type_decl :: Parser [Token] TypeDecl
parse_type_decl = [ TD_TypeAlias (TypeAlias name (map (\(T_Var x) -> x) params') meaning)
                      | Tk_Keyword "type" <- next,
                        T_Op (T_Concrete name) params' <- parse_type,
                        all (\p -> case p of { T_Var _ -> True; _ -> False }) params',
                        Tk_Symbol "=" <- next,
                        meaning <- parse_type,
                        Tk_Symbol ";" <- next ]
              ||| [ TD_TypeAlias (TypeAlias name [] meaning)
                      | Tk_Keyword "type" <- next,
                        T_Concrete name <- parse_type,
                        Tk_Symbol "=" <- next,
                        meaning <- parse_type,
                        Tk_Symbol ";" <- next ]
              ||| [ TD_ADT (ADT name (map (\(T_Var x) -> x) params') constructors)
                      | Tk_Keyword "data" <- next,
                        T_Op (T_Concrete name) params' <- parse_type,
                        all (\p -> case p of { T_Var _ -> True; _ -> False }) params',
                        Tk_Symbol "=" <- next,
                        constructors <- [ Constructor name args | (T_Op (T_Concrete name) args) <- parse_type ]
                                          `sepby` (element $ Tk_Symbol "|"),
                        Tk_Symbol ";" <- next ]
              ||| [ TD_ADT (ADT name [] constructors)
                      | Tk_Keyword "data" <- next,
                        T_Concrete name <- parse_type,
                        Tk_Symbol "=" <- next,
                        constructors <- ([ Constructor name args | (T_Op (T_Concrete name) args) <- parse_type ]
                                         ||| [ Constructor name [] | T_Concrete name <- parse_type ])
                                          `sepby` (element $ Tk_Symbol "|"),
                        Tk_Symbol ";" <- next ]

parse_statement :: Parser [Token] Stmt
parse_statement = first $ list_or [
                    parse_block,
                    parse_skip,
                    parse_if,
                    parse_while,
                    parse_assign,
                    parse_declare,
                    (parse_funcall "all" <^^ (element $ Tk_Symbol ";")) |> S_FunCall,
                    parse_return,
                    parse_match parse_statement |> S_Match,
                    parse_let_stmt
                  ]

parse_skip :: Parser [Token] Stmt
parse_skip = [ S_Skip | Tk_Keyword "skip" <- next,
                        Tk_Symbol ";" <- next ]

parse_if :: Parser [Token] Stmt
parse_if = [ S_If e b1 b2 | Tk_Keyword "if" <- next,
                            Tk_Symbol "(" <- next,
                            e <- parse_expr "all",
                            Tk_Symbol ")" <- next,
                            b1 <- parse_block,
                            b2 <- ([ b2 | Tk_Keyword "else" <- next, b2 <- parse_block ] |||| (return $ S_Block [])) ]

parse_while :: Parser [Token] Stmt
parse_while = [ S_While e b | Tk_Keyword "while" <- next,
                              Tk_Symbol "(" <- next,
                              e <- parse_expr "all",
                              Tk_Symbol ")" <- next,
                              b <- parse_block ]

parse_return :: Parser [Token] Stmt
parse_return = [ S_Return e | Tk_Keyword "return" <- next,
                              e <- possibly (parse_expr "all") |>
                                (\me -> case me of
                                  Just e  -> e
                                  Nothing -> E_Lit L_Unit
                                ),
                              Tk_Symbol ";" <- next ]

parse_assign :: Parser [Token] Stmt
parse_assign = [ S_Assign a e | a <- parse_access "all",
                                Tk_Symbol "=" <- next,
                                e <- parse_expr "all",
                                Tk_Symbol ";" <- next ]

parse_declare :: Parser [Token] Stmt
parse_declare = [ S_Declare t x e | t <- parse_type,
                                    x <- parse_identifier,
                                    Tk_Symbol "=" <- next,
                                    e <- parse_expr "all",
                                    Tk_Symbol ";" <- next ]

parse_block :: Parser [Token] Stmt
parse_block = [ S_Block stmts | Tk_Symbol "{" <- next,
                                stmts <- many parse_statement,
                                Tk_Symbol "}" <- next ]

parse_let_stmt :: Parser [Token] Stmt
parse_let_stmt = [ S_Let x e s | Tk_Keyword "let" <- next,
                                 Tk_Ident x <- next,
                                 Tk_Symbol "=" <- next,
                                 e <- parse_expr "all",
                                 Tk_Keyword "in" <- next,
                                 s <- parse_statement  ]



{-
  Parsing expressions
  ===================
-}

parse_binop :: [String] -> Parser [Token] (Expr -> Expr -> Expr)
parse_binop ops = [ E_BinOp op | Tk_Op op <- next, op `elem` ops ]

cond_parse :: (String -> Bool) -> (String -> Parser a b) -> (String -> Parser a b)
cond_parse test cp = \s -> if test s then cp s else mzero

parse_expr :: String -> Parser [Token] Expr
parse_expr = \s -> first $ list_or [
               parse_cons s,

               parse_or s,
               parse_and s,
               parse_not s,

               parse_eq s,
               parse_comp s,

               parse_add s,
               parse_mul s,
               parse_neg s,

               parse_closed s
             ]

parse_cons :: String -> Parser [Token] Expr
parse_cons = \s -> chainr1 (parse_cons_up s) (parse_binop [":"])

parse_cons_up :: String -> Parser [Token] Expr
parse_cons_up = \s -> first $ list_or [
                  parse_or s,
                  parse_and s,
                  parse_not s,
                  parse_eq s,
                  parse_comp s,
                  parse_closed s
                ]

parse_or :: String -> Parser [Token] Expr
parse_or = cond_parse (/= "match") $ \s ->
  chainr1 (parse_or_up s) (parse_binop ["||"])

parse_or_up :: String -> Parser [Token] Expr
parse_or_up = \s -> first $ list_or [
                parse_and s,
                parse_not s,
                parse_eq s,
                parse_comp s,
                parse_closed s
              ]

parse_and :: String -> Parser [Token] Expr
parse_and = cond_parse (/= "match") $ \s ->
  chainr1 (parse_and_up s) (parse_binop ["&&"])

parse_and_up :: String -> Parser [Token] Expr
parse_and_up = \s -> first $ list_or [
                 parse_not s,
                 parse_eq s,
                 parse_comp s,
                 parse_closed s
                ]

parse_not :: String -> Parser [Token] Expr
parse_not = cond_parse (/= "match") $ \s ->
  [ foldr E_UnOp e (map (const "!") ops) | ops <- many1 (element (Tk_Op "!")),
                                                     e <- parse_not_up s ]

parse_not_up :: String -> Parser [Token] Expr
parse_not_up = \s -> first $ list_or [
                 parse_eq s,
                 parse_comp s,
                 parse_closed s
                ]


parse_eq :: String -> Parser [Token] Expr
parse_eq = cond_parse (/= "match") $ \s ->
  chainr1 (parse_eq_up s) (parse_binop ["==", "!="])

parse_eq_up :: String -> Parser [Token] Expr
parse_eq_up = \s -> first $ list_or [
                parse_comp s,
                parse_add s,
                parse_mul s,
                parse_neg s,
                parse_closed s
              ]

parse_comp :: String -> Parser [Token] Expr
parse_comp = cond_parse (/= "match") $ \s ->
  chainr1 (parse_comp_up s) (parse_binop ["<", ">", "<=", ">="])

parse_comp_up :: String -> Parser [Token] Expr
parse_comp_up = \s -> first $ list_or [
                  parse_add s,
                  parse_mul s,
                  parse_neg s,
                  parse_closed s
                ]


parse_add :: String -> Parser [Token] Expr
parse_add = cond_parse (/= "match") $ \s ->
  chainl1 (parse_add_up s) (parse_binop ["+", "-"])

parse_add_up :: String -> Parser [Token] Expr
parse_add_up = \s -> first $ list_or [
                 parse_mul s,
                 parse_neg s,
                 parse_closed s
                ]

parse_mul :: String -> Parser [Token] Expr
parse_mul = cond_parse (/= "match") $ \s ->
  chainl1 (parse_mul_up s) (parse_binop ["*", "/", "%"])

parse_mul_up :: String -> Parser [Token] Expr
parse_mul_up = \s -> first $ list_or [
                 parse_neg s,
                 parse_closed s
                ]

parse_neg :: String -> Parser [Token] Expr
parse_neg = cond_parse (/= "match") $ \s ->
  [ foldr E_UnOp e (map (const "~") ops) | ops <- many1 (element (Tk_Op "-") |||| element (Tk_Op "~")),
                                           e <- parse_neg_up s ]

parse_neg_up :: String -> Parser [Token] Expr
parse_neg_up = \s -> first $ list_or [
                 parse_closed s
                ]

parse_access :: String -> Parser [Token] Access
parse_access "match" = [ Ident id | Tk_Ident id <- next ]
parse_access "all" =  [ foldl FieldAccess (Ident id) fields
                        | Tk_Ident id <- next,
                          fields <- many [ f | Tk_Symbol "." <- next,
                                                Tk_Keyword f <- next,
                                                f `elem` ["hd", "tl", "fst", "snd"] ]
                      ]

parse_funcall :: String -> Parser [Token] FunCall
parse_funcall = \s -> [ FunCall id (if length args == 0 then [E_Lit L_Unit] else args)
                         | Tk_Ident id <- next,
                           Tk_Symbol "(" <- next,
                           args <- parse_expr s `sepby` (element $ Tk_Symbol ","),
                           Tk_Symbol ")" <- next
                      ]

parse_closed :: String -> Parser [Token] Expr
parse_closed = \s -> first $ list_or [
    -- character literal
    [ E_Lit (L_Int $ ord c) | Tk_Char c <- next ],
    -- string literal
    [ foldr (E_BinOp ":") (E_Lit L_EmptyList) (map (E_Lit . L_Int . ord) s) | Tk_String s <- next ],
    -- int literal
    [ E_Lit (L_Int n)  | Tk_Int n  <- next ],
    -- bool literal
    [ E_Lit (L_Bool b) | Tk_Bool b <- next ],
    -- unit literal
    [ E_Lit L_Unit | Tk_Symbol "(" <- next, Tk_Symbol ")" <- next ],
    -- list
    [ foldr (E_BinOp ":") (E_Lit L_EmptyList) contents
          | Tk_Symbol "[" <- next,
            contents <- parse_expr s `sepby` (element $ Tk_Symbol ","),
            Tk_Symbol "]" <- next ],
    -- function declaration
    if s == "match" then mzero else
    [ E_Fun params b | Tk_Keyword "fun" <- next,
                       Tk_Symbol "(" <- next,
                       params <- parse_identifier `sepby` (element $ Tk_Symbol ","),
                       Tk_Symbol ")" <- next,
                       b <- parse_block ],
    -- let
    if s == "match" then mzero else
    [ E_Let id e1 e2 | Tk_Keyword "let" <- next,
                       Tk_Ident id <- next,
                       Tk_Symbol "=" <- next,
                       e1 <- parse_expr s,
                       Tk_Keyword "in" <- next,
                       e2 <- parse_expr s ],
    (parse_funcall s |> E_FunCall),
    (parse_access s |> E_Access),
    -- tuple
    [ E_Tuple e1 e2 | Tk_Symbol "(" <- next,
                     e1 <- parse_expr s,
                     Tk_Symbol "," <- next,
                     e2 <- parse_expr s,
                     Tk_Symbol ")" <- next ],
    -- match
    if s == "match" then mzero else
    (parse_match (parse_expr s) |> E_Match),
    if s /= "match" then mzero else
    [ E_MatchWildcard | Tk_MatchWildcard <- next ],
    [ e | Tk_Symbol "(" <- next, e <- parse_expr s, Tk_Symbol ")" <- next ]
  ]



{-
  Parsing types
  =============
-}

parse_type :: Parser [Token] Type
parse_type = parse_t |> transform
  where
    transform :: Type -> Type
    transform x = x

parse_t :: Parser [Token] Type
parse_t =
  -- this (first $) can be removed to allow backtracking, and ability to parse
  --  @parser s a x = some_parser;
  -- but when removed the language is still functional, though you must write:
  --  (@parser s a) x = some_parser;
  first $
  list_or [
    parse_t_fun,
    parse_t_app,

    parse_t_closed
  ]

parse_t_fun :: Parser [Token] Type
parse_t_fun = chainr1 parse_t_fun_up [ typeFun | _ <- element (Tk_Symbol "->") ]

parse_t_fun_up :: Parser [Token] Type
parse_t_fun_up = first $ list_or [
    parse_t_app,

    parse_t_closed
  ]

parse_t_app :: Parser [Token] Type
parse_t_app = [ T_Op t args | t <- parse_t_closed,
                              Tk_Symbol "(" <- next,
                              args <- parse_t `sepby` (element $ Tk_Symbol "," ),
                              Tk_Symbol ")" <- next ]

parse_t_closed :: Parser [Token] Type
parse_t_closed = first $ list_or [
    [ T_Var id        | Tk_Symbol "'" <- next,
                        id <- parse_identifier ],
    [ T_Concrete id   | id <- parse_identifier ],
    [ typeTuple t1 t2 | Tk_Symbol "(" <- next,
                        t1 <- parse_t,
                        Tk_Symbol "," <- next,
                        t2 <- parse_t,
                        Tk_Symbol ")" <- next
    ],
    [ typeList t      | Tk_Symbol "[" <- next,
                        t <- parse_t,
                        Tk_Symbol "]" <- next
    ],
    [ t             | Tk_Symbol "(" <- next,
                      t <- parse_t,
                      Tk_Symbol ")" <- next
    ]
  ]

parse_abstractions :: Parser [Token] [String]
parse_abstractions = [ abs | Tk_Keyword "forall" <- next,
                             abs <- many1 parse_identifier,
                             Tk_Symbol "." <- next ]
                |||| (return [])

parse_type_scheme :: Parser [Token] TypeScheme
parse_type_scheme = [ TypeScheme abs t | abs <- parse_abstractions,
                                         t <- parse_type ]
                ||| [ s | Tk_Symbol "(" <- next,
                          s <- parse_type_scheme,
                          Tk_Symbol ")" <- next ]

parse_type_env :: Parser [Token] TypeEnv
parse_type_env = (
    sepby
    [ (x, t) | t <- parse_type_scheme,
               x <- parse_identifier ]
    (element $ Tk_Symbol ",")
  ) |> Map.fromList |> TypeEnv

parse_subst :: Parser [Token] TypeSubst
parse_subst = (
    sepby
    [ (x, t) | x <- parse_identifier,
               Tk_Symbol ":=" <- next,
               t <- parse_type ]
    (element $ Tk_Symbol ",")
  ) |> Map.fromList |> TypeSubst
