{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, FlexibleInstances #-}

module Spla.Desugar where
  -- desugar :: Program -> Program



import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State

-- import qualified Text.PrettyPrint as PP
import Debug.Trace

import Utils

import Spla.Language


class Desugarable a where
  desugar :: a -> a


instance Desugarable Program where
  desugar (Program typedecls stmts) = Program typedecls (map desugar stmts)

instance Desugarable Stmt where
  desugar (S_Match m) = S_Match (desugar m)
  desugar s           = s

instance Desugarable Expr where
  desugar (E_Match m) = E_Match (desugar m)
  desugar e           = e

instance Desugarable (Match a) where
  desugar (Match e rules) = Match e (map desugar rules)

exprComplexity :: Expr -> Int
exprComplexity (E_Tuple e1 e2)              = 1 + max (exprComplexity e1) (exprComplexity e2)
exprComplexity (E_FunCall (FunCall _ args)) = 1 + foldl max 0 (map exprComplexity args)
exprComplexity (E_BinOp _ e1 e2)            = 1 + max (exprComplexity e1) (exprComplexity e2)
exprComplexity (E_UnOp _ e)                 = 1 + exprComplexity e
exprComplexity _ = 0

instance Desugarable (MatchRule a) where
  desugar r@(MatchRule (E_Tuple e1 e2) to)
{-    | exprComplexity e1 > 0 = MatchRule (E_Tuple (E_Access (Ident "fresh")) e2)
                                        Match (E_Access (Ident "fresh")) [
                                          MatchRule e1 to
                                        ]-}
    | otherwise = r
  desugar r = r
