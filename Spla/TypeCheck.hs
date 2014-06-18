{-# LANGUAGE MonadComprehensions #-}

-- http://www.grabmueller.de/martin/www/pub/AlgorithmW.pdf
-- http://fsharpcode.blogspot.nl/2010/08/hindley-milner-type-inference-sample.html

module Spla.TypeCheck where
  -- staticCheck :: AST_Program -> Program
  -- typeCheck :: Program -> Program   -- checks & infers types


-- TODO undefinedness of variables !!!

import Data.Maybe
import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State

-- import qualified Text.PrettyPrint as PP
import Debug.Trace

import Utils

import Spla.Language



staticCheck :: Program -> [String]
staticCheck p = [] {-testall [
    -- check for existence of "main" with correct type
    (\(Program _ stmts) -> case find isMainFnDeclare stmts of
      Nothing -> ["doesn't contain 'main' function at toplevel"]
      Just (S_Declare t _ e) -> case e of
        (E_Fun params s) ->
          if t == T_Fun (T_Basic "unit") (T_Basic "int")
            then []
            else ["'main' must be of type (unit -> int)"]
        _ -> ["'main' must be a function"]
    ),
    -- check if every function returns
    (\(Program _ stmts) -> concat $ map everyFnReturns stmts)
  ] p

everyFnReturns :: Stmt -> [String]
everyFnReturns (S_Declare _ x (E_Fun params s)) =
  if returns s then [] else ["function '" ++ x ++ "' doesn't neccessarily return"]
  ++
  everyFnReturns s
everyFnReturns _ = []

testall :: [a -> [b]] -> a -> [b]
testall ps x = concat $ map (\p -> p x) ps

isMainFnDeclare (S_Declare _ "main" e) = True
isMainFnDeclare _ = False-}

class StaticCheckJudgeable a where
  returns :: a -> Bool

instance StaticCheckJudgeable Stmt where
  returns (S_Block stmts) = any returns stmts
  returns (S_Return _)    = True
  returns (S_If _ b1 b2)  = returns b1 && returns b2
  returns _               = False


data TCState = TCState {
    -- map of alias name -> (params, meaning)
    aliasResolve :: Map.Map String ([String], Type)
  }

emptyTCState :: TCState
emptyTCState = TCState {
    aliasResolve = Map.empty
  }


type TC a = State TCState a


typeCheck :: Program -> Program
typeCheck p = p


discoverTypeAliases :: [TypeAliasDecl] -> State TCState ()
discoverTypeAliases [] = return ()
-- args is a list of T_Var's
discoverTypeAliases ((TypeAliasDecl (TypeAlias aliasName aliasParams) meaning):rest) = do
  knownAliases <- gets aliasResolve
  -- first check that it doesn't already exist
  case Map.lookup aliasName knownAliases of
    Just _ -> error $ "type alias already exists: " ++ aliasName
    Nothing ->
      let params = map (\(T_Var id) -> id) aliasParams in
        -- check that params match up with free vars of type alias
        if ftv meaning /= Set.fromList params
          then error $ "type alias params do not match up with free vars"
          else do
            realMeaning <- resolveTypeAlias meaning
            modify $ \s -> s { aliasResolve = Map.insert aliasName (params, realMeaning) knownAliases }
            -- continue
            discoverTypeAliases rest


resolveTypeAlias :: Type -> State TCState Type
resolveTypeAlias (T_Alias (TypeAlias aliasName args)) = do
  knownAliases <- gets aliasResolve
  case Map.lookup aliasName knownAliases of
    Nothing -> error $ "type alias not known: " ++ aliasName
    Just (params, meaning) -> if length params /= length args
      then error $ "type alias not given the right number of args: " ++ aliasName
      else return $ subst_tv (TypeSubst . Map.fromList $ zip params args) meaning

resolveTypeAlias (T_Tuple t1 t2) = [ T_Tuple t1' t2' | t1' <- resolveTypeAlias t1, t2' <- resolveTypeAlias t2 ]
resolveTypeAlias (T_Fun t1 t2)   = [ T_Fun t1' t2'   | t1' <- resolveTypeAlias t1, t2' <- resolveTypeAlias t2 ]
resolveTypeAlias (T_List t)      = [ T_List t'       | t' <- resolveTypeAlias t ]
resolveTypeAlias t               = return t
