{-# LANGUAGE MonadComprehensions #-}

-- For testing purposes...

module Spla where

import System.Environment
import System.Process

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace

import Data.List

import System.FilePath

import Utils
import ParserCombinators

import Spla.Language

import Spla.Lexer      --(splalex)
import Spla.Parser     --(splaparse,     parse_expr, parse_type, parse_type_env, parse_type_scheme, parse_subst)

import Spla.Desugar    --(desugar)

--import Spla.TypeCheck  --(typeCheck)

import qualified Spla.SSMCompile  as SSM  --(compile)
--import qualified Spla.LLVMCompile as LLVM --(compile)


main = do
  [filename] <- getArgs
  testCompileWrite filename


test t f = [ t code | code <- readFile f ]

--testStaticCheck    = test (staticCheck . parseProgram)
--testTypeCheck      = test (typeCheck . parseProgram)

testLex            = test splalex
testLexCount       = test (length . splalex)

testParse          = test parseProgram
testParseCount     = test (length . parseProgramCode)
testParseTwice     = test ((\ps -> ps == (concat $ map (parseProgramCode . show) ps)) . parseProgramCode)

testDesugar        = test (desugar . parseProgram)

testCompile        = test (SSM.compile . parseProgram)
testCompileWrite f = do
  code <- readFile f
  let program = parseProgram code in
    writeFile (addExtension f "ssm") (SSM.compile program)

parseExpr :: String -> Expr
parseExpr = head . map fst . concat . map (parse (successful $ parse_expr "all")) . splalex

parseMatchExpr :: String -> Expr
parseMatchExpr = head . map fst . concat . map (parse (successful $ parse_expr "match")) . splalex

parseStmt :: String -> Stmt
parseStmt = head . map fst . concat . map (parse (successful parse_statement)) . splalex

parseProgramCode :: String -> [AST_Program]
parseProgramCode = map fst . concat . map (parse splaparse) . splalex

parseProgram :: String -> AST_Program
parseProgram = head . parseProgramCode

--parseType :: String -> Type
parseType = map fst . concat . map (parse $ successful parse_type) . splalex
parseT = map fst . concat . map (parse $ successful parse_t) . splalex

parseTypeDecl :: String -> TypeDecl
parseTypeDecl = head . map fst . concat . map (parse $ successful parse_type_decl) . splalex

parseTypeScheme :: String -> TypeScheme
parseTypeScheme = head . map fst . concat . map (parse $ successful parse_type_scheme) . splalex

parseTypeEnv :: String -> TypeEnv
parseTypeEnv = head . map fst . concat . map (parse $ successful parse_type_env) . splalex

parseTypeSubst :: String -> TypeSubst
parseTypeSubst = head . map fst . concat . map (parse $ successful parse_subst) . splalex
