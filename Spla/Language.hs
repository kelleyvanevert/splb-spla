{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, InstanceSigs #-}

{-
  Contents:
  - Lexical stuff
  - Operators and their types
  - Tokens
  - Abstract syntax tree (and operations)
  - Type theory
-}

module Spla.Language where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set



{-
  Misc
  ====
-}

class ShowNeedsBrackets a where
  showNeedsBrackets :: a -> Bool

showBracket :: (ShowNeedsBrackets a, Show a) => a -> String
showBracket x = if showNeedsBrackets x then "(" ++ show x ++ ")" else show x



{-
  Lexical stuff
  =============
-}

basic_types :: [String]
basic_types = ["unit", "bool", "int"]

booleans :: [String]
booleans = ["true", "false"]

whitespace :: [Char]
whitespace = [' ', '\n', '\r']

keywords :: [String]
keywords = [
  "data",
  "match",
  "type",
  "forall",
  "fun",
  "return",
  "skip",
  "if", "else", "while",
  "hd", "tl", "fst", "snd",
  "let", "in" ]

symbols :: [String]
symbols = ["->", ":=", "=", ";", ",", ".", "|", "'",
  "(", ")", "{", "}", "[", "]"]



{-
  Operators and their types
  =========================
-}

fieldOperatorTypes :: [(String, Type)]
fieldOperatorTypes = [
    ("fst", typeFun (typeTuple (T_Var "a") (T_Var "b")) (T_Var "a")),
    ("snd", typeFun (typeTuple (T_Var "a") (T_Var "b")) (T_Var "b")),
    ("hd",  typeFun (typeList (T_Var "a")) (T_Var "a")),
    ("tl",  typeFun (typeList (T_Var "a")) (typeList (T_Var "a")))
  ]

operatorTypes :: [(String, Type)]
operatorTypes = [
    ("*", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "int"))),
    ("/", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "int"))),
    ("%", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "int"))),
    ("+", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "int"))),
    ("-", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "int"))),

    ("~", typeFun (T_Concrete "int") (T_Concrete "int")),

    ("<=", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "bool"))),
    (">=", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "bool"))),
    ("==", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "bool"))),
    ("!=", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "bool"))),
    (">", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "bool"))),
    ("<", typeFun (T_Concrete "int") (typeFun (T_Concrete "int") (T_Concrete "bool"))),

    ("&&", typeFun (T_Concrete "bool") (typeFun (T_Concrete "bool") (T_Concrete "bool"))),
    ("||", typeFun (T_Concrete "bool") (typeFun (T_Concrete "bool") (T_Concrete "bool"))),

    ("!", typeFun (T_Concrete "bool") (T_Concrete "bool")),

    (":", typeFun (T_Var "b") (typeFun (typeList (T_Var "b")) (typeList (T_Var "b"))))
  ]

operators :: [String]
operators = map fst operatorTypes


languageEnv :: TypeEnv
languageEnv = TypeEnv $ Map.fromList [
    ("print", TypeScheme ["t"] $ typeFun (T_Var "t") (T_Concrete "unit")),
    ("isEmpty", TypeScheme ["t"] $ typeFun (typeList (T_Var "t")) (T_Concrete "bool"))
  ]



{-
  Tokens
  ======
-}

data Token = Tk_Ident String
           | Tk_Keyword String
           | Tk_Symbol String
           | Tk_Int Int
           | Tk_Char Char
           | Tk_String String
           | Tk_Unit
           | Tk_Bool Bool
           | Tk_Op String
           | Tk_MatchWildcard
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


class LexicalVars a where
  fv :: a -> Set.Set String
  subst :: ExprSubst -> a -> a

instance LexicalVars a => LexicalVars [a] where
  subst s = map (subst s)
  fv l = foldr Set.union Set.empty (map fv l)


data ExprSubst = ExprSubst (Map.Map String String)
  deriving (Eq, Ord)

instance Show ExprSubst where
  show (ExprSubst s) = intercalate ", " $
    map (\(x, t) -> x ++ " := " ++ (show t)) (Map.assocs s)

substInsert :: String -> String -> ExprSubst -> ExprSubst
substInsert k t (ExprSubst s) = ExprSubst (Map.insert k t s)

substRemove :: String -> ExprSubst -> ExprSubst
substRemove k (ExprSubst s) = ExprSubst (Map.delete k s)

nullExprSubst :: ExprSubst
nullExprSubst = ExprSubst Map.empty

singletonExprSubst :: String -> String -> ExprSubst
singletonExprSubst k v = ExprSubst $ Map.fromList [(k, v)]

{-composeExprSubst :: ExprSubst -> ExprSubst -> ExprSubst
composeExprSubst (ExprSubst s1) (ExprSubst s2) =
  ExprSubst ((Map.map (subst (ExprSubst s1)) s2) `Map.union` s1)
-}

data Access = Ident String
            | FieldAccess Access String
  deriving (Eq, Ord)

instance Show Access where
  show (Ident id) = id
  show (FieldAccess a field) = (show a) ++ "." ++ field

instance LexicalVars Access where
  fv :: Access -> Set.Set String
  fv (Ident id) = Set.singleton id
  fv (FieldAccess a field) = fv a

  subst :: ExprSubst -> Access -> Access
  subst (ExprSubst s) (Ident id) = case Map.lookup id s of
                                      Just id' -> Ident id'
                                      Nothing  -> Ident id
  subst s (FieldAccess a field) = FieldAccess (subst s a) field

explodeAccess :: Access -> (String, [String])
explodeAccess (Ident id) = (id, [])
explodeAccess (FieldAccess a d) =
  let (id, ds) = explodeAccess a in (id, d:ds)


data Match a = Match Expr [MatchRule a]
  deriving (Eq, Ord)

instance (Show a) => Show (Match a) where
  show (Match e rules) = "match " ++ (show e) ++ " {\n" ++
    (intercalate "\n" (map show rules)) ++
    "\n}"

instance (LexicalVars a) => LexicalVars (Match a) where
  fv (Match e rules) = (fv e) `Set.union` (Set.unions $ map fv rules)
  subst s (Match e rules) = Match (subst s e) (map (subst s) rules)


data MatchRule a = MatchRule Expr a
  deriving (Eq, Ord)

instance (Show a) => Show (MatchRule a) where
  show (MatchRule e to) = "  | " ++ (show e) ++ " -> " ++ (show to)

instance (LexicalVars a) => LexicalVars (MatchRule a) where
  fv (MatchRule e to) = (fv to) `Set.difference` (fv e)
  subst s (MatchRule e to) = MatchRule e (subst s' to)
    where s' = foldl (.) (\x -> x) (map substRemove $ Set.elems (fv e)) $ s


data Expr = E_Access Access
          | E_Lit Lit
          | E_Fun [String] Stmt
          | E_BinOp String Expr Expr
          | E_UnOp String Expr
          | E_Tuple Expr Expr
          | E_Let String Expr Expr
          | E_FunCall FunCall
          | E_Match (Match Expr)
          | E_MatchWildcard
  deriving (Eq, Ord)

instance Show Expr where
  show (E_Access a)       = show a
  show (E_Lit lit)        = show lit
  show (E_Fun params b)   = "fun (" ++ (intercalate ", " params) ++ ")\n" ++ (show b)
  show (E_BinOp op e1 e2) = (showBracket e1) ++ " " ++ op ++ " " ++ (showBracket e2)
  show (E_UnOp op e)      = op ++ (showBracket e)
  show (E_Tuple e1 e2)    = "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"
  show (E_Let id e1 e2)   = "let " ++ id ++ " = " ++ (show e1) ++ " in " ++ (show e2)
  show (E_FunCall fc)     = show fc
  show (E_Match m)        = show m
  show (E_MatchWildcard)  = "_"

instance ShowNeedsBrackets Expr where
  showNeedsBrackets (E_BinOp _ _ _) = True
  showNeedsBrackets _               = False

instance LexicalVars Expr where
  fv (E_Access a)      = fv a
  fv (E_BinOp _ e1 e2) = (fv e1) `Set.union` (fv e2)
  fv (E_UnOp op e)     = fv e
  fv (E_Tuple e1 e2)   = (fv e1) `Set.union` (fv e2)
  fv (E_Lit lit)       = Set.empty
  fv (E_FunCall f)     = fv f
  fv (E_Let id e1 e2)  = (fv e1) `Set.union` ((fv e2) `Set.difference` (Set.singleton id))
  fv (E_Fun params b)  = (fv b) `Set.difference` (Set.fromList params)
  fv (E_Match m)       = fv m
  fv (E_MatchWildcard) = Set.empty
  
  subst s (E_Access a)       = E_Access (subst s a)
  subst s (E_BinOp op e1 e2) = (E_BinOp op (subst s e1) (subst s e2))
  subst s (E_UnOp op e)      = (E_UnOp op (subst s e))
  subst s (E_Tuple e1 e2)    = (E_Tuple (subst s e1) (subst s e2))
  subst s (E_Lit lit)        = (E_Lit lit)
  subst s (E_FunCall f)      = (E_FunCall (subst s f))
  subst s (E_Fun params b)   = E_Fun params (subst s' b)
    where s' = foldl (.) (\x -> x) (map substRemove params) $ s
  subst (ExprSubst s) (E_Let id e1 e2) = (E_Let id (subst (ExprSubst s) e1) (subst (ExprSubst s') e2))
    where s' = Map.delete id s
  subst s (E_Match m)        = E_Match (subst s m)
  subst s (E_MatchWildcard)  = E_MatchWildcard


data Lit = L_Int Int
         | L_Bool Bool
         | L_EmptyList
         | L_Unit
  deriving (Eq, Ord)

instance Show Lit where
  show (L_Int n)     = show n
  show (L_Bool b)    = if b then "true" else "false"
  show (L_EmptyList) = "[]"
  show (L_Unit)      = "()"


data TypeDecl = TypeAliasDecl Type Type
              | ADTDecl Type [Type]
  deriving Eq

instance Show TypeDecl where
  show (TypeAliasDecl alias meaning) = "type " ++ (show alias) ++ " = " ++ (show meaning) ++ ";"
  show (ADTDecl adt constructions) =
    let x = "data " ++ (show adt) ++ " "
        len = length x
    in
      x ++ "= " ++ (intercalate ("\n" ++ replicate len ' ' ++ "| ") $ map show constructions) ++ ("\n" ++ replicate len ' ' ++ ";")


data Program = Program [TypeDecl] [Stmt]
  deriving Eq

instance Show Program where
  show (Program tds stmts) =
    (intercalate "\n\n" (map show tds)) ++ "\n\n" ++
    (intercalate "\n\n" (map show stmts))


data Stmt = S_Declare Type String Expr
          | S_Block [Stmt]
          | S_If Expr Stmt Stmt
          | S_While Expr Stmt
          | S_Assign Access Expr
          | S_FunCall FunCall
          | S_Return Expr
          | S_Skip
          | S_Match (Match Stmt)
          | S_Asm [String]
  deriving (Eq, Ord)

instance Show Stmt where
  show (S_Declare t s e) = showBracket t ++ " " ++ s ++ " = " ++ (show e) ++ ";"
  show (S_Block stmts)   = "{\n" ++ (indent $ intercalate "\n" $ map show stmts) ++ "\n" ++ "}"
  show (S_If e b1 b2)    = "if (" ++ (show e) ++ ")\n" ++ (show b1) ++ "\nelse\n" ++ (show b2)
  show (S_While e b)     = "while (" ++ (show e) ++ ")\n" ++ (show b)
  show (S_Assign a e)    = (show a) ++ " = " ++ (show e) ++ ";"
  show (S_Return e)      = "return " ++ (showBracket e) ++ ";"
  show (S_FunCall fc)    = (show fc) ++ ";"
  show S_Skip            = "skip;"

instance LexicalVars Stmt where
  fv (S_Declare t x e) = fv e
  fv b@(S_Block stmts) = (Set.unions $ map fv stmts) `Set.difference` (Set.fromList $ blockVars b)
  fv (S_If e b1 b2)    = (fv e) `Set.union` (fv b1) `Set.union` (fv b2)
  fv (S_While e b)     = (fv e) `Set.union` (fv b)
  fv (S_Assign a e)    = (fv a) `Set.union` (fv e)
  fv (S_FunCall f)     = fv f
  fv (S_Return e)      = fv e
  fv (S_Skip)          = Set.empty

  subst s (S_Declare t x e) = S_Declare t x (subst s e)
  subst s b@(S_Block stmts) = S_Block (map (subst s') stmts)
    where s' = foldl (.) (\x -> x) (map substRemove (blockVars b)) $ s
  subst s (S_If e b1 b2)    = S_If (subst s e) (subst s b1) (subst s b2)
  subst s (S_While e b)     = S_While (subst s e) (subst s b)
  subst s (S_Assign a e)    = S_Assign (subst s a) (subst s e)
  subst s (S_FunCall f)     = S_FunCall (subst s f)
  subst s (S_Return e)      = S_Return (subst s e)
  subst s (S_Skip)          = S_Skip

blockVars :: Stmt -> [String]
blockVars (S_Block stmts) = foldr (\stmt vars ->
    case stmt of
      S_Declare _ x _ -> x:vars
      otherwise       -> vars
  ) [] stmts


data FunCall = FunCall String [Expr]
  deriving (Eq, Ord)

instance Show FunCall where
  show (FunCall id es) = id ++ "(" ++ (intercalate ", " $ map show es) ++ ")"

instance LexicalVars FunCall where
  fv (FunCall _ args) = Set.unions $ map fv args
  subst s (FunCall a args) = FunCall a (map (subst s) args)



{-
  Type theory
  ===========
-}

class LexicalTypeVars a where
  ftv :: a -> Set.Set String
  subst_tv :: TypeSubst -> a -> a

instance LexicalTypeVars a => LexicalTypeVars [a] where
  subst_tv s = map (subst_tv s)
  ftv l = foldr Set.union Set.empty (map ftv l)


data TypeSubst = TypeSubst (Map.Map String Type)
  deriving (Eq, Ord)

instance Show TypeSubst where
  show (TypeSubst s) = intercalate ", " $
    map (\(x, t) -> x ++ " := " ++ (show t)) (Map.assocs s)

nullTypeSubst :: TypeSubst
nullTypeSubst = TypeSubst Map.empty

composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst (TypeSubst s1) (TypeSubst s2) =
  TypeSubst ((Map.map (subst_tv (TypeSubst s1)) s2) `Map.union` s1)


data Type = T_Concrete String
          | T_Op Type [Type]
          | T_Var String
  deriving (Eq, Ord)

instance Show Type where
  show (T_Concrete s)                       = s
  show (T_Op (T_Concrete "tuple") [t1, t2]) = intercalate "" ["(", show t1, ", ", show t2, ")"]
  show (T_Op (T_Concrete "list") [t])       = intercalate "" ["[", show t, "]"]
  show (T_Op (T_Concrete "fun") [t1, t2])   = showBracket t1 ++ " -> " ++ showBracket t2
  show (T_Op t args)                        = showBracket t ++ "(" ++ (intercalate ", " $ map show args) ++ ")"
  show (T_Var x)                            = "'" ++ x

instance ShowNeedsBrackets Type where
  showNeedsBrackets (T_Op _ _)   = True
  showNeedsBrackets _            = False

typeFun :: Type -> Type -> Type
typeFun t1 t2 = T_Op (T_Concrete "fun") [t1, t2]

typeList :: Type -> Type
typeList t = T_Op (T_Concrete "list") [t]

typeTuple :: Type -> Type -> Type
typeTuple t1 t2 = T_Op (T_Concrete "tuple") [t1, t2]


data TypeScheme = TypeScheme [String] Type
  deriving (Eq, Ord)

instance Show TypeScheme where
  show (TypeScheme abs t) =
    if length abs == 0
    then (show t)
    else "forall " ++ (intercalate " " abs) ++ ". " ++ (show t)

instance ShowNeedsBrackets TypeScheme where
  showNeedsBrackets _ = False

instance LexicalTypeVars Type where
  ftv (T_Concrete x) = Set.empty
  ftv (T_Op t args)  = (ftv t) `Set.union` (Set.unions $ map ftv args)
  ftv (T_Var x)      = Set.singleton x

  subst_tv s (T_Concrete x) = T_Concrete x
  subst_tv s (T_Op t args)  = T_Op (subst_tv s t) (map (subst_tv s) args)
  subst_tv (TypeSubst s) (T_Var x) =
    case Map.lookup x s of
      Just t -> t
      Nothing -> T_Var x

instance LexicalTypeVars TypeScheme where
  ftv (TypeScheme vars t) = (ftv t) `Set.difference` (Set.fromList vars)
  subst_tv (TypeSubst s) (TypeScheme vars t) = TypeScheme vars (subst_tv (TypeSubst (foldr Map.delete s vars)) t)

data TypeEnv = TypeEnv (Map.Map String TypeScheme)
  deriving (Eq, Ord)

nullTypeEnv :: TypeEnv
nullTypeEnv = TypeEnv Map.empty

instance Show TypeEnv where
  show (TypeEnv env) = intercalate ", " $
    map (\(x, s) -> (showBracket s) ++ " " ++ x) (Map.assocs env)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

typeEnvUnion :: TypeEnv -> TypeEnv -> TypeEnv
typeEnvUnion (TypeEnv g1) (TypeEnv g2) = TypeEnv (g1 `Map.union` g2)

instance LexicalTypeVars TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  subst_tv s (TypeEnv env) = TypeEnv (Map.map (subst_tv s) env)
