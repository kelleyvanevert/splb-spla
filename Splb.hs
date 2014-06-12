{-# LANGUAGE MonadComprehensions #-}

-- For testing purposes...

module Splb where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace

import Data.List

import System.FilePath

import Utils
import ParserCombinators

import Splb.Language

import Splb.Lexer      (splblex)
import Splb.Parser     (splbparse,     parse_expr, parse_type, parse_type_env, parse_type_scheme, parse_subst)

import Splb.TypeCheck  (typecheck)

import qualified Splb.SSMCompile  as SSM  (compile)
import qualified Splb.LLVMCompile as LLVM (compile)


--testTypeCheck f = [ evalState (typeCheck (parseProgram code)) 0 | code <- readFile f ]


parseExpression :: String -> Expr
parseExpression = head . map fst . concat . map (parse (successful parse_expr)) . splblex

parseProgramCode :: String -> [Program]
parseProgramCode = map fst . concat . map (parse splbparse) . splblex

parseProgram :: String -> Program
parseProgram = head . map fst . concat . map (parse splbparse) . splblex

parseType :: String -> Type
parseType = head . map fst . concat . map (parse $ successful parse_type) . splblex

parseTypeScheme :: String -> TypeScheme
parseTypeScheme = head . map fst . concat . map (parse $ successful parse_type_scheme) . splblex

parseTypeEnv :: String -> TypeEnv
parseTypeEnv = head . map fst . concat . map (parse $ successful parse_type_env) . splblex

parseTypeSubst :: String -> TypeSubst
parseTypeSubst = head . map fst . concat . map (parse $ successful parse_subst) . splblex

testParse f = [ parseProgramCode code | code <- readFile f ]
testParseTwice f = [ parseProgramCode code == (concat $ map (parseProgramCode . show) $ parseProgramCode code) | code <- readFile f ]

testCompile f = [ SSM.compile (parseProgram code) | code <- readFile f ]
testCompileWrite f = do
  code <- readFile f
  let program = parseProgram code in
    writeFile (addExtension f "ssm") (SSM.compile program)
