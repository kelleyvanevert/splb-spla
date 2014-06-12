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
  "type",
  "forall",
  "var", "fun",
  "return",
  "skip",
  "if", "else", "while",
  "hd", "tl", "fst", "snd",
  "let", "in" ]

symbols :: [String]
symbols = ["->", ":=", "=", "@", ";", ",", ".",
  "(", ")", "{", "}", "[", "]"]



{-
  Operators and their types
  =========================
-}

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


data Expr = E_Access Access
          | E_Lit Lit
          | E_Fun [String] Stmt
          | E_BinOp String Expr Expr
          | E_UnOp String Expr
          | E_Tuple Expr Expr
          | E_Let String Expr Expr
          | E_FunCall FunCall
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


data TypeAliasDecl = TypeAliasDecl TypeAlias Type
  deriving Eq

instance Show TypeAliasDecl where
  show (TypeAliasDecl alias meaning) = "type " ++ (show alias) ++ " = " ++ (show meaning) ++ ";"


data Program = Program [TypeAliasDecl] [Stmt]
  deriving Eq

instance Show Program where
  show (Program tds stmts) =
    (intercalate "\n" (map show tds)) ++ "\n\n" ++
    (intercalate "\n\n" (map show stmts))


data Stmt = S_Declare Type String Expr
          | S_Block [Stmt]
          | S_If Expr Stmt Stmt
          | S_While Expr Stmt
          | S_Assign Access Expr
          | S_FunCall FunCall
          | S_Return Expr
          | S_Skip
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


data TypeAlias = TypeAlias String [Type]
  deriving (Eq, Ord)

instance Show TypeAlias where
  show (TypeAlias s args) = "@" ++ s ++ (if (length args > 0) then (" " ++ intercalate " " (map showBracket args)) else "")

instance LexicalTypeVars TypeAlias where
  ftv (TypeAlias _ args) = Set.unions (map ftv args)
  subst_tv s (TypeAlias t args)  = TypeAlias t (map (subst_tv s) args)


data Type = T_Basic String
          | T_Tuple Type Type
          | T_List Type
          | T_Var String
          | T_Alias TypeAlias
          | T_Fun Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (T_Basic s)     = s
  show (T_Tuple t1 t2) = intercalate "" ["(", show t1, ", ", show t2, ")"]
  show (T_List t)      = intercalate "" ["[", show t, "]"]
  show (T_Var x)       = x
  show (T_Alias t)     = show t
  show (T_Fun t1 t2)   = showBracket t1 ++ " -> " ++ showBracket t2

instance ShowNeedsBrackets Type where
  showNeedsBrackets (T_Alias _)   = True
  showNeedsBrackets (T_Fun _ _)   = True
  showNeedsBrackets _             = False

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
  ftv (T_Basic s)     = Set.empty
  ftv (T_Tuple t1 t2) = (ftv t1) `Set.union` (ftv t2)
  ftv (T_List t)      = ftv t
  ftv (T_Var x)       = Set.singleton x
  ftv (T_Alias t)     = ftv t
  ftv (T_Fun t1 t2)   = (ftv t1) `Set.union` (ftv t2)

  subst_tv s (T_Tuple t1 t2)  = T_Tuple (subst_tv s t1) (subst_tv s t2)
  subst_tv s (T_List t)       = T_List (subst_tv s t)
  subst_tv s (T_Fun t1 t2)    = T_Fun (subst_tv s t1) (subst_tv s t2)
  subst_tv (TypeSubst s) (T_Var x) =
    case Map.lookup x s of
      Just t -> t
      Nothing -> T_Var x
  subst_tv s (T_Alias t)      = T_Alias (subst_tv s t)
  subst_tv s t                = t

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
