{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, InstanceSigs, FlexibleContexts #-}

{-
  Contents:
  - Lexical stuff
  - Operators and their types
  - Tokens
  - Abstract syntax tree (and operations)
  - Type theory
-}

module Spla.LexicalVars where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set



{-
  [LexicalVars a b]
    means that [a] has lexical variables,
    where the variables, and variable
    substitutions, live in [b]
-}

type VarSubst a = Map.Map a a

composeVarSubst :: LexicalVars a a => VarSubst a -> VarSubst a -> VarSubst a
composeVarSubst s1 s2 = (Map.map (subst s1) s2) `Map.union` s1


class Ord b => LexicalVars a b where
  fv :: a -> Set.Set b
  subst :: VarSubst b -> a -> a

instance LexicalVars a b => LexicalVars [a] b where
  fv l = Set.unions (map fv l)
  subst s = map (subst s)


data Let a = Let String Expr a
  deriving (Eq, Ord)

instance LexicalVars a Expr => LexicalVars (Let a) Expr where
  fv (Let x e h) = (fv e) `Set.union` (Set.delete (E_Var x) $ fv h)
  subst s (Let x e h) = (Let x (subst s e) (subst s' h))
    where s' = Map.delete (E_Var x) s


data Match a = Match Expr [MatchRule a]
  deriving (Eq, Ord)

instance LexicalVars a Expr => LexicalVars (Match a) Expr where
  fv (Match e rules) = (fv e) `Set.union` (Set.unions $ map fv rules)
  subst s (Match e rules) = Match (subst s e) (map (subst s) rules)


data MatchRule a = MatchRule Expr a
  deriving (Eq, Ord)

instance LexicalVars a Expr => LexicalVars (MatchRule a) Expr where
  fv (MatchRule e to) = (fv to) `Set.difference` (fv e)
  subst s (MatchRule e to) = MatchRule e (subst s' to)
    where s' = foldl (.) (\x -> x) (map Map.delete $ Set.elems (fv e)) $ s


data Expr = E_Var String
          | E_Access Expr String
          | E_Lit Lit
          | E_Fun [String] Stmt
          | E_BinOp String Expr Expr
          | E_UnOp String Expr
          | E_Tuple Expr Expr
          | E_Let (Let Expr)
          | E_FunCall FunCall
          | E_Data Data
          | E_Match (Match Expr)
          | E_MatchWildcard
  deriving (Eq, Ord)

instance LexicalVars Expr Expr where
  fv (E_Var x)          = Set.singleton (E_Var x)
  fv (E_Access e field) = fv e
  fv (E_BinOp _ e1 e2)  = (fv e1) `Set.union` (fv e2)
  fv (E_UnOp op e)      = fv e
  fv (E_Tuple e1 e2)    = (fv e1) `Set.union` (fv e2)
  fv (E_Lit lit)        = Set.empty
  fv (E_FunCall f)      = fv f
  fv (E_Let l)          = fv l
  fv (E_Fun params b)   = (fv b) `Set.difference` (Set.fromList $ map E_Var params)
  fv (E_Match m)        = fv m
  fv (E_Data d)         = fv d
  fv (E_MatchWildcard)  = Set.empty
  
  subst s (E_Var x)          = case Map.lookup (E_Var x) s of
                                 Just e' -> e'
                                 Nothing -> E_Var x
  subst s (E_BinOp op e1 e2) = (E_BinOp op (subst s e1) (subst s e2))
  subst s (E_UnOp op e)      = (E_UnOp op (subst s e))
  subst s (E_Tuple e1 e2)    = (E_Tuple (subst s e1) (subst s e2))
  subst s (E_Lit lit)        = (E_Lit lit)
  subst s (E_FunCall f)      = (E_FunCall (subst s f))
  subst s (E_Fun params b)   = E_Fun params (subst s' b)
    where s' = foldl (.) (\x -> x) (map Map.delete $ map E_Var params) $ s
  subst s (E_Let l)          = E_Let (subst s l)
  subst s (E_Match m)        = E_Match (subst s m)
  subst s (E_Data d)         = E_Data (subst s d)
  subst s (E_MatchWildcard)  = E_MatchWildcard

data Lit = L_Int Int
         | L_Bool Bool
         | L_EmptyList
         | L_Unit
  deriving (Eq, Ord)


{-
data ADT = ADT String [String] [Constructor]
  deriving (Eq, Ord)

instance Show ADT where
  show (ADT name params constructors) =
    let x = "data " ++ (show $ T_Op (T_Concrete name) (map T_Var params)) ++ " "
        len = length x
    in
      x ++ "= " ++ (intercalate ("\n" ++ replicate len ' ' ++ "| ") $ map show constructors) ++ ("\n" ++ replicate len ' ' ++ ";")


data Constructor = Constructor String [Type]
  deriving (Eq, Ord)

instance Show Constructor where
  show (Constructor name args) = if length args == 0
                                 then name
                                 else show $ T_Op (T_Concrete name) args


data TypeAlias = TypeAlias String [String] Type
  deriving (Eq, Ord)

instance Show TypeAlias where
  show (TypeAlias name params meaning) =
    "type " ++ (show $ T_Op (T_Concrete name) (map T_Var params)) ++ " = " ++ (show meaning) ++ ";"


data TypeDecl = TD_TypeAlias TypeAlias
              | TD_ADT ADT
  deriving (Eq, Ord)

instance Show TypeDecl where
  show (TD_TypeAlias alias) = show alias
  show (TD_ADT adt) = show adt


data Program = Program [TypeDecl] [Stmt]
  deriving (Eq, Ord)

instance Show Program where
  show (Program tds stmts) =
    (intercalate "\n\n" (map show tds)) ++ "\n\n" ++
    (intercalate "\n\n" (map show stmts))
-}

data Stmt = S_Declare 
                      String -- TODO Type
                             String Expr
          | S_Block [Stmt]
          | S_If Expr Stmt Stmt
          | S_While Expr Stmt
          | S_Assign Expr Expr
          | S_FunCall FunCall
          | S_Return Expr
          | S_Let (Let Stmt)
          | S_Skip
          | S_Match (Match Stmt)
          | S_Asm [String]
  deriving (Eq, Ord)

instance LexicalVars Stmt Expr where
  fv (S_Declare t x e) = fv e
  fv b@(S_Block stmts) = (Set.unions $ map fv stmts) `Set.difference` (Set.fromList $ map E_Var $ blockVars b)
  fv (S_If e b1 b2)    = (fv e) `Set.union` (fv b1) `Set.union` (fv b2)
  fv (S_While e b)     = (fv e) `Set.union` (fv b)
  fv (S_Assign a e)    = (fv a) `Set.union` (fv e)
  fv (S_FunCall f)     = fv f
  fv (S_Return e)      = fv e
  fv (S_Let l)         = fv l
  fv (S_Skip)          = Set.empty
  fv (S_Match m)       = fv m

  subst s (S_Declare t x e) = S_Declare t x (subst s e)
  subst s b@(S_Block stmts) = S_Block (map (subst s') stmts)
    where s' = foldl (.) (\x -> x) (map Map.delete (map E_Var $ blockVars b)) $ s
  subst s (S_If e b1 b2)    = S_If (subst s e) (subst s b1) (subst s b2)
  subst s (S_While e b)     = S_While (subst s e) (subst s b)
  subst s (S_Assign a e)    = S_Assign (subst s a) (subst s e)
  subst s (S_FunCall f)     = S_FunCall (subst s f)
  subst s (S_Return e)      = S_Return (subst s e)
  subst s (S_Let l)         = S_Let (subst s l)
  subst s (S_Skip)          = S_Skip
  subst s (S_Match m)       = S_Match (subst s m)

blockVars :: Stmt -> [String]
blockVars (S_Block stmts) = foldr (\stmt vars ->
    case stmt of
      S_Declare _ x _ -> x:vars
      otherwise       -> vars
  ) [] stmts


data FunCall = FunCall String [Expr]
  deriving (Eq, Ord)

instance LexicalVars FunCall Expr where
  fv (FunCall _ args) = Set.unions $ map fv args
  subst s (FunCall a args) = FunCall a (map (subst s) args)

data Data = Data String [Expr]
  deriving (Eq, Ord)

instance LexicalVars Data Expr where
  fv (Data _ args) = Set.unions $ map fv args
  subst s (Data a args) = Data a (map (subst s) args)




{-
  Type theory
  ===========
-}




data Type = T_Concrete String
          | T_Op Type [Type]
          | T_Var String
  deriving (Eq, Ord)

instance LexicalVars Type Type where
  fv (T_Concrete x) = Set.empty
  fv (T_Op t args)  = (fv t) `Set.union` (Set.unions $ map fv args)
  fv (T_Var x)      = Set.singleton (T_Var x)

  subst s (T_Concrete x) = T_Concrete x
  subst s (T_Op t args)  = T_Op (subst s t) (map (subst s) args)
  subst s (T_Var x) = case Map.lookup (T_Var x) s of
                        Just t' -> t'
                        Nothing -> T_Var x

typeFun :: Type -> Type -> Type
typeFun t1 t2 = T_Op (T_Concrete "fun") [t1, t2]

typeList :: Type -> Type
typeList t = T_Op (T_Concrete "list") [t]

typeTuple :: Type -> Type -> Type
typeTuple t1 t2 = T_Op (T_Concrete "tuple") [t1, t2]


data TypeScheme = TS_Type Type
                | TS_Abs Type Type
  deriving (Eq, Ord)

instance LexicalVars TypeScheme Type where
  fv (TS_Type t)   = fv t
  fv (TS_Abs x ts) = Set.delete x (fv ts)

  subst s (TS_Type t)   = TS_Type (subst s t)
  subst s (TS_Abs x ts) = TS_Abs x (subst s' ts)
    where s' = Map.delete x s

data TypeEnv = TypeEnv (Map.Map String TypeScheme)
  deriving (Eq, Ord)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

typeEnvUnion :: TypeEnv -> TypeEnv -> TypeEnv
typeEnvUnion (TypeEnv g1) (TypeEnv g2) = TypeEnv (g1 `Map.union` g2)

instance LexicalVars TypeEnv Type where
  fv (TypeEnv env) = fv (Map.elems env)
  subst s (TypeEnv env) = TypeEnv (Map.map (subst s) env)
