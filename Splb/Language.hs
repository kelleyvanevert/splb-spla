{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, InstanceSigs #-}

{-
  Defines the SPL language data structures
  - types
  - abstract syntax tree
-}

module Splb.Language where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set



basic_types :: [String]
basic_types = ["unit", "bool", "int"]

booleans :: [String]
booleans = ["true", "false"]

whitespace :: [Char]
whitespace = [' ', '\n', '\r']

keywords :: [String]
keywords = [
  "forall",
  "var", "fun",
  "return",
  "skip",
  "if", "else", "while",
  "hd", "tl", "fst", "snd",
  "let", "in" ]

symbols :: [String]
symbols = ["::", "->", ":=", ";", ",", ".",
  "(", ")", "{", "}", "[", "]"]

fieldOperatorTypes :: [(String, Type)]
fieldOperatorTypes = [
    ("fst", T_Fun (T_Tuple (T_Var "a") (T_Var "b")) (T_Var "a")),
    ("snd", T_Fun (T_Tuple (T_Var "a") (T_Var "b")) (T_Var "b")),
    ("hd",  T_Fun (T_List (T_Var "a")) (T_Var "a")),
    ("tl",  T_Fun (T_List (T_Var "a")) (T_List (T_Var "a")))
  ]

operatorTypes :: [(String, Type)]
operatorTypes = [
    ("*", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "int"))),
    ("/", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "int"))),
    ("%", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "int"))),
    ("+", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "int"))),
    ("-", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "int"))),

    ("~", T_Fun (T_Basic "int") (T_Basic "int")),

    ("<=", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "bool"))),
    (">=", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "bool"))),
    ("==", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "bool"))),
    ("!=", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "bool"))),
    (">", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "bool"))),
    ("<", T_Fun (T_Basic "int") (T_Fun (T_Basic "int") (T_Basic "bool"))),

    ("&&", T_Fun (T_Basic "bool") (T_Fun (T_Basic "bool") (T_Basic "bool"))),
    ("||", T_Fun (T_Basic "bool") (T_Fun (T_Basic "bool") (T_Basic "bool"))),

    ("!", T_Fun (T_Basic "bool") (T_Basic "bool")),

    (":", T_Fun (T_Var "b") (T_Fun (T_List (T_Var "b")) (T_List (T_Var "b"))))
  ]

operators :: [String]
operators = map fst operatorTypes


languageEnv :: TypeEnv
languageEnv = TypeEnv $ Map.fromList [
    ("print", TypeScheme ["t"] $ T_Fun (T_Var "t") (T_Basic "unit")),
    ("isEmpty", TypeScheme ["t"] $ T_Fun (T_List (T_Var "t")) (T_Basic "bool"))
  ]


data Token = Tk_Type String
           | Tk_Ident String
           | Tk_Keyword String
           | Tk_Symbol String
           | Tk_Int Int
           | Tk_Unit
           | Tk_Bool Bool
           | Tk_Op String
  deriving (Eq, Show, Read)


-- Ugly, but oh well
indent :: String -> String
indent s = "  " ++ (intercalate "" $ map f s)
  where
    f '\n' = "\n  "
    f c    = [c]


{-
  Abstract syntax tree (AST)
  ==========================
-}


class LexicalExprVars a where
  fev :: a -> Set.Set String
  esubst :: ExprSubst -> a -> a

instance LexicalExprVars a => LexicalExprVars [a] where
  esubst s = map (esubst s)
  fev l = foldr Set.union Set.empty (map fev l)


data ExprSubst = ExprSubst (Map.Map String String)
  deriving (Eq, Ord)

instance Show ExprSubst where
  show (ExprSubst s) = intercalate ", " $
    map (\(x, t) -> x ++ " := " ++ (show t)) (Map.assocs s)

esubstInsert :: String -> String -> ExprSubst -> ExprSubst
esubstInsert k t (ExprSubst s) = ExprSubst (Map.insert k t s)

nullExprSubst :: ExprSubst
nullExprSubst = ExprSubst Map.empty

singletonExprSubst :: String -> String -> ExprSubst
singletonExprSubst k v = ExprSubst $ Map.fromList [(k, v)]

{-composeExprSubst :: ExprSubst -> ExprSubst -> ExprSubst
composeExprSubst (ExprSubst s1) (ExprSubst s2) =
  ExprSubst ((Map.map (esubst (ExprSubst s1)) s2) `Map.union` s1)
-}

data Access = Ident String
            | FieldAccess Access String
  deriving (Eq, Ord)

instance Show Access where
  show (Ident id) = id
  show (FieldAccess a field) = (show a) ++ "." ++ field

instance LexicalExprVars Access where
  fev :: Access -> Set.Set String
  fev (Ident id) = Set.singleton id
  fev (FieldAccess a field) = fev a

  esubst :: ExprSubst -> Access -> Access
  esubst (ExprSubst s) (Ident id) = case Map.lookup id s of
                                      Just id' -> Ident id'
                                      Nothing  -> Ident id
  esubst s (FieldAccess a field) = FieldAccess (esubst s a) field


data Expr = E_Access Access
          | E_Lit Lit
          | E_BinOp String Expr Expr
          | E_UnOp String Expr
          | E_Tuple Expr Expr
          | E_Let String Expr Expr
          | E_FunCall FunCall
  deriving (Eq, Ord)

instance Show Expr where
  show (E_Access a)       = show a
  show (E_Lit lit)        = show lit
  show (E_BinOp op e1 e2) = "(" ++ (show e1) ++ " " ++ op ++ " " ++ (show e2) ++ ")"
  show (E_UnOp op e)      = op ++ (show e)
  show (E_Tuple e1 e2)    = "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"
  show (E_Let id e1 e2)   = "let " ++ id ++ " := " ++ (show e1) ++ " in " ++ (show e2)
  show (E_FunCall fc)     = show fc

instance LexicalExprVars Expr where
  fev (E_Access a)      = fev a
  fev (E_BinOp _ e1 e2) = (fev e1) `Set.union` (fev e2)
  fev (E_UnOp op e)     = fev e
  fev (E_Tuple e1 e2)   = (fev e1) `Set.union` (fev e2)
  fev (E_Lit lit)       = Set.empty
  fev (E_FunCall (FunCall _ args)) = Set.unions (map fev args)
  fev (E_Let id e1 e2)  = (fev e1) `Set.union` ((fev e2) `Set.difference` (Set.singleton id))

  esubst s (E_Access a)       = E_Access (esubst s a)
  esubst s (E_BinOp op e1 e2) = (E_BinOp op (esubst s e1) (esubst s e2))
  esubst s (E_UnOp op e)      = (E_UnOp op (esubst s e))
  esubst s (E_Tuple e1 e2)    = (E_Tuple (esubst s e1) (esubst s e2))
  esubst s (E_Lit lit)        = (E_Lit lit)
  esubst s (E_FunCall (FunCall id args)) = (E_FunCall (FunCall id (map (esubst s) args)))
  esubst (ExprSubst s) (E_Let id e1 e2) = (E_Let id (esubst (ExprSubst s) e1) (esubst (ExprSubst s') e2))
    where
      s' = Map.delete id s


data Lit = L_Int Int
         | L_Bool Bool
         | L_EmptyList
         | L_Unit
  deriving (Eq, Ord)

instance Show Lit where
  show (L_Int n)     = show n
  show (L_Bool b)    = if b then "true" else "false"
  show (L_EmptyList) = "[]"
  show (L_Unit)      = "#"


data Param = Param String Type
  deriving (Eq, Ord)

instance Show Param where
  show (Param id t) = (show t) ++ " " ++ id


data Var = Var String (Maybe Type)
  deriving (Eq, Ord)

instance Show Var where
  show (Var id mt) = case mt of
    Nothing -> id
    (Just t) -> show (Param id t)


data Program = Program [FunDecl]
  deriving Eq

instance Show Program where
  show (Program ds) = intercalate "\n\n" (map show ds)


-- return type, identifier, parameters, statements
data FunDecl = FunDecl Type String [Param] Block
  deriving Eq

instance Show FunDecl where
  show (FunDecl t id ps b) =
    (show t) ++ " " ++ id ++ " (" ++ (intercalate ", " $ map show ps) ++ ")\n" ++
    (show b)


data VarDecl = VarDecl String Type
  deriving (Eq, Ord)

instance Show VarDecl where
  show (VarDecl id t) = (show t) ++ " " ++ id ++ ";"


data Block = Block [VarDecl] [Stmt]
  deriving Eq

instance Show Block where
  show (Block vardecls stmts) =
    "{\n" ++
    (
      if (length vardecls > 0)
        then (indent $ intercalate "\n" $ map show vardecls) ++ "\n\n"
        else ""
    ) ++
    (indent $ intercalate "\n" $ map show stmts) ++ "\n" ++
    "}"


data Stmt = S_Block Block
          | S_If Expr Block (Maybe Block)
          | S_While Expr Block
          | S_Assign Access Expr
          | S_FunCall FunCall
          | S_Return (Maybe Expr)
          | S_Skip
  deriving Eq

instance Show Stmt where
  show (S_Block b) = show b
  show (S_If e b1 mb2) = "if (" ++ (show e) ++ ")\n" ++ (show b1) ++
    (case mb2 of { Nothing -> ""; Just b2 -> "\nelse\n" ++ (show b2) })
  show (S_While e b)  = "while (" ++ (show e) ++ ")\n" ++ (show b)
  show (S_Assign a e) = (show a) ++ " := " ++ (show e) ++ ";"
  show (S_Return e)   = "return" ++ (case e of { Nothing -> ""; Just e -> " " ++ (show e) }) ++ ";"
  show (S_FunCall fc) = (show fc) ++ ";"
  show S_Skip = "skip;"


data FunCall = FunCall String [Expr]
  deriving (Eq, Ord)

instance Show FunCall where
  show (FunCall id es) = id ++ "(" ++ (intercalate ", " $ map show es) ++ ")"


{-
  The type theory
  ===============
-}

class LexicalTypeVars a where
  fv :: a -> Set.Set String
  subst :: TypeSubst -> a -> a

instance LexicalTypeVars a => LexicalTypeVars [a] where
  subst s = map (subst s)
  fv l = foldr Set.union Set.empty (map fv l)


data TypeSubst = TypeSubst (Map.Map String Type)
  deriving (Eq, Ord)

instance Show TypeSubst where
  show (TypeSubst s) = intercalate ", " $
    map (\(x, t) -> x ++ " := " ++ (show t)) (Map.assocs s)

substInsert :: String -> Type -> TypeSubst -> TypeSubst
substInsert k t (TypeSubst s) = TypeSubst (Map.insert k t s)

nullTypeSubst :: TypeSubst
nullTypeSubst = TypeSubst Map.empty

composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst (TypeSubst s1) (TypeSubst s2) =
  TypeSubst ((Map.map (subst (TypeSubst s1)) s2) `Map.union` s1)


data Type = T_Basic String
          | T_Tuple Type Type
          | T_List Type
          | T_Var String
          | T_Fun Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (T_Basic s) = s
  show (T_Tuple t1 t2) = intercalate "" ["(", show t1, ", ", show t2, ")"]
  show (T_List t) = intercalate "" ["[", show t, "]"]
  show (T_Var s) = s
  show (T_Fun t1 t2) = intercalate "" ["(", show t1, " -> ", show t2, ")"]

data TypeScheme = TypeScheme [String] Type
  deriving (Eq, Ord)

instance Show TypeScheme where
  show (TypeScheme abs t) =
    if length abs == 0
    then (show t)
    else "forall " ++ (intercalate " " abs) ++ ". " ++ (show t)

instance LexicalTypeVars Type where
  fv (T_Basic s)     = Set.empty
  fv (T_Tuple t1 t2) = (fv t1) `Set.union` (fv t2)
  fv (T_List t)      = fv t
  fv (T_Var s)       = Set.singleton s
  fv (T_Fun t1 t2)   = (fv t1) `Set.union` (fv t2)

  subst s (T_Tuple t1 t2)   = T_Tuple (subst s t1) (subst s t2)
  subst s (T_List t)        = T_List (subst s t)
  subst s (T_Fun t1 t2)     = T_Fun (subst s t1) (subst s t2)
  subst (TypeSubst s) (T_Var a) = case Map.lookup a s of
                                Just t -> t
                                Nothing -> T_Var a
  subst s t                 = t

instance LexicalTypeVars TypeScheme where
  fv (TypeScheme vars t) = (fv t) `Set.difference` (Set.fromList vars)
  subst (TypeSubst s) (TypeScheme vars t) = TypeScheme vars (subst (TypeSubst (foldr Map.delete s vars)) t)

data TypeEnv = TypeEnv (Map.Map String TypeScheme)
  deriving (Eq, Ord)

nullTypeEnv :: TypeEnv
nullTypeEnv = TypeEnv Map.empty

instance Show TypeEnv where
  show (TypeEnv env) = intercalate ", " $
    map (\(x, s) -> x ++ " :: " ++ (show s)) (Map.assocs env)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

typeEnvUnion :: TypeEnv -> TypeEnv -> TypeEnv
typeEnvUnion (TypeEnv g1) (TypeEnv g2) = TypeEnv (g1 `Map.union` g2)

instance LexicalTypeVars TypeEnv where
  fv (TypeEnv env) = fv (Map.elems env)
  subst s (TypeEnv env) = TypeEnv (Map.map (subst s) env)
