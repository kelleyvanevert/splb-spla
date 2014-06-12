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
  Misc
  ====
-}

parse_identifier :: Parser [Token] String
parse_identifier = [ id | Tk_Ident id <- next ]



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

parse_type_decl :: Parser [Token] TypeAliasDecl
parse_type_decl = [ TypeAliasDecl t1 t2 | Tk_Keyword "type" <- next,
                                          (T_Alias t1) <- parse_type,
                                          Tk_Symbol "=" <- next,
                                          t2 <- parse_type,
                                          Tk_Symbol ";" <- next ]

parse_statement :: Parser [Token] Stmt
parse_statement = first $ list_or [
                    parse_block,
                    parse_skip,
                    parse_if,
                    parse_while,
                    parse_assign,
                    parse_declare,
                    (parse_funcall <^^ (element $ Tk_Symbol ";")) |> S_FunCall,
                    parse_return
                  ]

parse_skip :: Parser [Token] Stmt
parse_skip = [ S_Skip | Tk_Keyword "skip" <- next,
                        Tk_Symbol ";" <- next ]

parse_if :: Parser [Token] Stmt
parse_if = [ S_If e b1 b2 | Tk_Keyword "if" <- next,
                            Tk_Symbol "(" <- next,
                            e <- parse_expr,
                            Tk_Symbol ")" <- next,
                            b1 <- parse_block,
                            b2 <- ([ b2 | Tk_Keyword "else" <- next, b2 <- parse_block ] |||| (return $ S_Block [])) ]

parse_while :: Parser [Token] Stmt
parse_while = [ S_While e b | Tk_Keyword "while" <- next,
                              Tk_Symbol "(" <- next,
                              e <- parse_expr,
                              Tk_Symbol ")" <- next,
                              b <- parse_block ]

parse_return :: Parser [Token] Stmt
parse_return = [ S_Return e | Tk_Keyword "return" <- next,
                              e <- possibly parse_expr |> (\list -> case list of
                                                              e : _ -> e
                                                              []    -> E_Lit L_Unit
                                                            ),
                              Tk_Symbol ";" <- next ]

parse_assign :: Parser [Token] Stmt
parse_assign = [ S_Assign a e | a <- parse_access,
                                Tk_Symbol "=" <- next,
                                e <- parse_expr,
                                Tk_Symbol ";" <- next ]

parse_declare :: Parser [Token] Stmt
parse_declare = [ S_Declare t x e | t <- parse_type,
                                    x <- parse_identifier,
                                    Tk_Symbol "=" <- next,
                                    e <- parse_expr,
                                    Tk_Symbol ";" <- next ]

parse_block :: Parser [Token] Stmt
parse_block = [ S_Block stmts | Tk_Symbol "{" <- next,
                                stmts <- many parse_statement,
                                Tk_Symbol "}" <- next ]



{-
  Parsing expressions
  ===================
-}

parse_binop :: [String] -> Parser [Token] (Expr -> Expr -> Expr)
parse_binop ops = [ E_BinOp op | Tk_Op op <- next, op `elem` ops ]

parse_expr :: Parser [Token] Expr
parse_expr = first $ list_or [
               parse_cons,

               parse_or,
               parse_and,
               parse_not,

               parse_eq,
               parse_comp,

               parse_add,
               parse_mul,
               parse_neg,

               parse_closed
             ]

parse_cons :: Parser [Token] Expr
parse_cons = chainr1 parse_cons_up (parse_binop [":"])

parse_cons_up :: Parser [Token] Expr
parse_cons_up = first $ list_or [
                  parse_or,
                  parse_and,
                  parse_not,
                  parse_eq,
                  parse_comp,
                  parse_closed
                ]

parse_or :: Parser [Token] Expr
parse_or = chainr1 parse_or_up (parse_binop ["||"])

parse_or_up :: Parser [Token] Expr
parse_or_up = first $ list_or [
                parse_and,
                parse_not,
                parse_eq,
                parse_comp,
                parse_closed
              ]

parse_and :: Parser [Token] Expr
parse_and = chainr1 parse_and_up (parse_binop ["&&"])

parse_and_up :: Parser [Token] Expr
parse_and_up = first $ list_or [
                 parse_not,
                 parse_eq,
                 parse_comp,
                 parse_closed
                ]

parse_not :: Parser [Token] Expr
parse_not = [ foldr E_UnOp e (map (const "!") ops) | ops <- many1 (element (Tk_Op "!")),
                                                     e <- parse_not_up ]

parse_not_up :: Parser [Token] Expr
parse_not_up = first $ list_or [
                 parse_eq,
                 parse_comp,
                 parse_closed
                ]


parse_eq :: Parser [Token] Expr
parse_eq = chainr1 parse_eq_up (parse_binop ["==", "!="])

parse_eq_up :: Parser [Token] Expr
parse_eq_up = first $ list_or [
                parse_comp,
                parse_add,
                parse_mul,
                parse_neg,
                parse_closed
              ]

parse_comp :: Parser [Token] Expr
parse_comp = chainr1 parse_comp_up (parse_binop ["<", ">", "<=", ">="])

parse_comp_up :: Parser [Token] Expr
parse_comp_up = first $ list_or [
                  parse_add,
                  parse_mul,
                  parse_neg,
                  parse_closed
                ]


parse_add :: Parser [Token] Expr
parse_add = chainl1 parse_add_up (parse_binop ["+", "-"])

parse_add_up :: Parser [Token] Expr
parse_add_up = first $ list_or [
                 parse_mul,
                 parse_neg,
                 parse_closed
                ]

parse_mul :: Parser [Token] Expr
parse_mul = chainl1 parse_mul_up (parse_binop ["*", "/", "%"])

parse_mul_up :: Parser [Token] Expr
parse_mul_up = first $ list_or [
                 parse_neg,
                 parse_closed
                ]

parse_neg :: Parser [Token] Expr
parse_neg = [ foldr E_UnOp e (map (const "~") ops) | ops <- many1 (element (Tk_Op "-") |||| element (Tk_Op "~")),
                                                     e <- parse_neg_up ]

parse_neg_up :: Parser [Token] Expr
parse_neg_up = first $ list_or [
                 parse_closed
                ]

parse_access :: Parser [Token] Access
parse_access = [ foldl FieldAccess (Ident id) fields
                  | Tk_Ident id <- next,
                    fields <- many [ f | Tk_Symbol "." <- next,
                                          Tk_Keyword f <- next,
                                          f `elem` ["hd", "tl", "fst", "snd"] ]
                ]

parse_funcall :: Parser [Token] FunCall
parse_funcall = [ FunCall id args
                   | Tk_Ident id <- next,
                     Tk_Symbol "(" <- next,
                     args <- parse_expr `sepby` (element $ Tk_Symbol ","),
                     Tk_Symbol ")" <- next
                ]

parse_closed :: Parser [Token] Expr
parse_closed = first $ list_or [
                [ E_Lit (L_Int $ ord c) | Tk_Char c <- next ],
                [ foldr (E_BinOp ":") (E_Lit L_EmptyList) (map (E_Lit . L_Int . ord) s) | Tk_String s <- next ],
                [ E_Fun params b | Tk_Keyword "fun" <- next,
                                   Tk_Symbol "(" <- next,
                                   params <- parse_identifier `sepby` (element $ Tk_Symbol ","),
                                   Tk_Symbol ")" <- next,
                                   b <- parse_block ],
                [ E_Let id e1 e2 | Tk_Keyword "let" <- next,
                                   Tk_Ident id <- next,
                                   Tk_Symbol "=" <- next,
                                   e1 <- parse_expr,
                                   Tk_Keyword "in" <- next,
                                   e2 <- parse_expr ],
                [ e | Tk_Symbol "(" <- next, e <- parse_expr, Tk_Symbol ")" <- next ],
                [ E_Lit (L_Int n)  | Tk_Int n  <- next ],
                [ E_Lit (L_Bool b) | Tk_Bool b <- next ],
                parse_funcall |> E_FunCall,
                parse_access |> E_Access,
                [ E_Lit L_EmptyList | Tk_Symbol "[" <- next, Tk_Symbol "]" <- next ],
                [ E_Lit L_Unit | Tk_Symbol "(" <- next, Tk_Symbol ")" <- next ],
                [ E_Tuple e1 e2 | Tk_Symbol "(" <- next,
                                 e1 <- parse_expr,
                                 Tk_Symbol "," <- next,
                                 e2 <- parse_expr,
                                 Tk_Symbol ")" <- next ]
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
parse_t_fun = chainr1 parse_t_fun_up [ T_Fun | _ <- element (Tk_Symbol "->") ]

parse_t_fun_up :: Parser [Token] Type
parse_t_fun_up = first $ list_or [
    parse_t_app,

    parse_t_closed
  ]

-- TODO correctly parser @parser @string @string
--  instead of having to do @parser (@string) (@string)
--  (forcing parse_t_closed)
parse_t_app :: Parser [Token] Type
parse_t_app = [ T_Alias (TypeAlias id args) | Tk_Symbol "@" <- next,
                                              id <- parse_identifier,
                                              args <- many parse_t_fun_up ]

parse_t_closed :: Parser [Token] Type
parse_t_closed = first $ list_or [
    [ T_Basic id    | id <- parse_identifier, id `elem` basic_types ],
    [ T_Var id      | id <- parse_identifier ],
    [ T_Tuple t1 t2 | Tk_Symbol "(" <- next,
                      t1 <- parse_t,
                      Tk_Symbol "," <- next,
                      t2 <- parse_t,
                      Tk_Symbol ")" <- next
    ],
    [ T_List t      | Tk_Symbol "[" <- next,
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
