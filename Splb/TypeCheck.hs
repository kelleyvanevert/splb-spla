{-# LANGUAGE MonadComprehensions #-}

-- http://www.grabmueller.de/martin/www/pub/AlgorithmW.pdf
-- http://fsharpcode.blogspot.nl/2010/08/hindley-milner-type-inference-sample.html

module Splb.TypeCheck where
  -- typecheck :: Program -> Program


-- TODO undefinedness of variables !!!

import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

-- import qualified Text.PrettyPrint as PP
import Debug.Trace

import Utils

import Splb.Language


type TC a = State Int a


-- TODO
typecheck :: Program -> Program
typecheck p = if evalState (typeCheck p) 0
              then p
              else error "Program does not type check!"


typeCheck :: Program -> TC Bool
typeCheck p =
  let functions = programFunctions p in
    let toplevelEnv = (traceShowId $ funTypes functions) `typeEnvUnion` languageEnv in

      case findNonReturningFn functions of
        Just id -> error $ "Non-returning function: " ++ id
        otherwise ->
          {-
            foldM :: Monad (TC Bool) =>
                     (Bool -> FunDecl -> TC Bool) ->
                      Bool ->
                             [FunDecl] ->
                                         TC Bool
          -}
          foldM (
            \ok1 (FunDecl tau _ params b) ->
              let funEnv = (paramsToEnv params) `typeEnvUnion` toplevelEnv in do
                ok2 <- ok funEnv tau b
                return $ ok1 && ok2
          ) True (Map.elems functions)
      where
        findNonReturningFn functions =
          let idReturns = Map.map (\(FunDecl _ i _ b) -> (i, returns b)) functions in
            Map.fold
              (\(FunDecl _ i _ b) ret -> maybe (if returns b then Nothing else Just i) Just ret)
              Nothing
              functions


class Judgeable a where
  returns :: a -> Bool

  -- [ok gamma tau a]
  -- judges whether, given an environment gamma:
  --   [a] type checks, (possibly) returning something of type [tau]
  ok :: TypeEnv -> Type -> a -> TC Bool


instance Judgeable Stmt where
  returns (S_Block b)           = returns b
  returns (S_Return _)          = True
  returns (S_If _ b1 (Just b2)) = returns b1 && returns b2
  returns _                     = False

  {-
        gamma |- b  OK[tau]
    ———————————————————————————
    gamma |- S_Block b  OK[tau]
  -}
  ok gamma tau (S_Block b) = ok gamma tau b

  {-
    gamma |- e :: bool     gamma |- b  OK[tau]
    ——————————————————————————————————————————
         gamma |- S_While e b  OK[tau]
  -}
  ok gamma tau (S_While e b) = do
    (eType, s) <- infer gamma e
    case eType of
      T_Basic "bool" -> ok gamma tau b
      otherwise -> error $ "While condition (" ++ (show e) ++ ") has erroneous type (" ++ (show eType) ++ ")!"

  {-
    gamma |- e :: bool     gamma |- b  OK[tau]
    ——————————————————————————————————————————
         gamma |- S_If e b Nothing  OK[tau]
  -}
  ok gamma tau (S_If e b Nothing) = do
    (eType, s) <- infer gamma e
    case eType of
      T_Basic "bool" -> ok gamma tau b
      otherwise -> error $ "If condition (" ++ (show e) ++ ") has erroneous type (" ++ (show eType) ++ ")!"

  {-
    gamma |- e :: bool     gamma |- b1  OK[tau]     gamma |- b2  OK[tau]
    ————————————————————————————————————————————————————————————————————
                  gamma |- S_If e b1 (Just b2)  OK[tau]
  -}
  ok gamma tau (S_If e b1 (Just b2)) = do
    (eType, s) <- infer gamma e
    case eType of
      T_Basic "bool" -> do
        ok1 <- ok gamma tau b1
        ok2 <- ok gamma tau b2
        return $ ok1 && ok2
      otherwise -> error $ "If condition (" ++ (show e) ++ ") has erroneous type (" ++ (show eType) ++ ")!"

  {-
    ————————————————————————————————————
    gamma |- S_Return Nothing   OK[unit]
  -}
  ok _ tau (S_Return Nothing) = return $
    if tau == (T_Basic "unit")
      then True
      else error $ "Return expression has type (unit) instead of expected type " ++ (show tau)

  {-
          gamma |- e :: tau
    —————————————————————————————
    gamma |- S_Return e   OK[tau]
  -}
  ok gamma tau (S_Return (Just e)) = do
    (eType, s) <- infer gamma e
    if tau == eType
      then return True
      else error $ "Return expression (" ++ (show e) ++ ") has type (" ++ (show eType) ++ ") instead of expected type " ++ (show tau)

  {-
    gamma |- a :: tau'     gamma |- e :: tau'
    —————————————————————————————————————————
          gamma |- S_Assign a e  OK[tau]
  -}
  ok gamma tau (S_Assign a e) = do
    (aType, aS) <- infer gamma (E_Access a)
    (eType, eS) <- infer gamma e
    traceShowM aS
    traceShowM eS
    if subst eS aType == subst eS eType
      then return True
      else error $
        "Assignment type error: variable " ++ (show a) ++ " has type (" ++ (show aType) ++ ")" ++
        " but expression (" ++ (show e) ++ ") has type (" ++ (show eType) ++ ")." ++
        " BTW: as=("++(show aS)++") es=(" ++ (show eS)++")"

  {-
          gamma |- f :: tau'
    —————————————————————————————
    gamma |- S_FunCall f  OK[tau]
  -}
  ok gamma tau (S_FunCall f) = do
    (feType, s) <- infer gamma (E_FunCall f)
    return True


instance Judgeable Block where
  returns (Block _ stmts) = foldr ((||) . returns) False stmts

  {-
    forall i [  gamma, [t v] |- stmt_i  OK[tau]  ]
    ——————————————————————————————————————————————
      gamma |- Block [t v] { stmt_i }i  OK[tau]
  -}
  ok gamma tau (Block vardecls stmts) = do
    let gamma' = (varDeclsToEnv vardecls) `typeEnvUnion` gamma in
      {-
        -- ok (TypeEnv g') tau stmt
        -- foldr ((&&) . ok gamma tau) True stmts
        foldM :: Monad (TC Bool) =>
                 (Bool -> Stmt -> TC Bool) ->
                  Bool ->
                         [Stmt] ->
                                  TC Bool
      -}
      foldM (\b stmt -> [ b && b' | b' <- ok gamma' tau stmt ]) True stmts



paramsToEnv :: [Param] -> TypeEnv
paramsToEnv params =
  let kvs = map (\(Param id t) -> (id, TypeScheme [] t)) params in
    case findDuplicate (map fst kvs) of
      Just id -> error $ "Duplicate param identifier: " ++ id
      Nothing -> TypeEnv $ Map.fromList kvs


varDeclsToEnv :: [VarDecl] -> TypeEnv
varDeclsToEnv params =
  let kvs = map (\(VarDecl id t) -> (id, TypeScheme [] t)) params in
    case findDuplicate (map fst kvs) of
      Just id -> error $ "Duplicate identifier in block variable declarations: " ++ id
      Nothing -> TypeEnv $ Map.fromList kvs


findDuplicate :: Ord a => [a] -> (Maybe a)
findDuplicate xs = dup xs Set.empty
  where
    dup :: Ord a => [a] -> Set.Set a -> (Maybe a)
    dup [] _ = Nothing
    dup (x:xs) s = if Set.member x s
                    then Just x
                    else dup xs (Set.insert x s)



-- Restructures a program to an (identifier -> function declaration) map,
--  checks for duplicate function identifiers,
--  checks for a "main" function of type (unit -> int),
--  checks first-order requirements (TODO)
programFunctions :: Program -> Map.Map String FunDecl
programFunctions (Program funDecls) =
  let id_fs = map (\f@(FunDecl _ id _ _) -> (id, f)) $ funDecls in
    case findDuplicate (map fst id_fs) of
      Just id -> error $ "Duplicate function identifier: " ++ id
      Nothing ->
        let funMap = Map.fromList id_fs in
          if Map.member "isEmpty" funMap
            then error $ "Redefining (isEmpty) is not allowed!"
            else
              if Map.member "print" funMap
                then error $ "Redefining (isEmpty) is not allowed!"
                else
                  case Map.lookup "main" funMap of
                    Nothing -> error $ "No (main) function present!"
                    Just funDecl -> let t = funType funDecl in
                      if t /= TypeScheme [] (T_Fun (T_Basic "unit") (T_Basic "int"))
                        then error $ "Main function has type (" ++ (show t) ++ ") instead of required type (unit -> int)!"
                        else funMap


-- Gets the function types of a program
funTypes :: (Map.Map String FunDecl) -> TypeEnv
funTypes p = TypeEnv $ Map.map funType p


-- Gets the function type of a FunDecl,
--  checks FO restriction
funType :: FunDecl -> TypeScheme
funType (FunDecl t id params b) =
  case t of
    T_Fun _ _ -> error $ "Function return type not FO: " ++ (show t)
    otherwise ->
      let paramTypes = case params of { [] -> [T_Basic "unit"]; _ -> (map paramType params) } in
        let ungeneralizedType = foldr T_Fun t paramTypes in
          TypeScheme (Set.toList $ fv ungeneralizedType) ungeneralizedType
  where
    paramType (Param pid pt) = case pt of
      T_Fun _ _ -> error $ "Function param type not FO: " ++ (show pt) ++ " " ++ pid ++ " in function " ++ id
      otherwise -> pt




generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = TypeScheme vars t
  where vars = Set.toList ((fv t) `Set.difference` (fv env))

mgu :: Type -> Type -> TypeSubst
mgu a b = mgu' a b nullTypeSubst
  where
    mgu' a b s = case (subst s a, subst s b) of
      -- basic
      (T_Var ta,T_Var tb)      | ta == tb -> s
      (T_Basic ta, T_Basic tb) | ta == tb -> s
      -- occurs check
      (T_Var ta,   _) | ta `Set.notMember` (fv b) -> substInsert ta b s
      -- switch
      (_, T_Var _) -> mgu' b a s
      -- recurse
      (T_List ta, T_List tb) -> mgu' ta tb s
      (T_Tuple ta1 ta2, T_Tuple tb1 tb2) -> mgu' ta1 tb1 (mgu' ta2 tb2 s)
      (T_Fun ta1 ta2, T_Fun tb1 tb2) -> mgu' ta1 tb1 (mgu' ta2 tb2 s)
      -- not unifiable
      (x, y) -> error $ "Could not unify types: " ++ (show x) ++ " vs. " ++ (show y)

newTyVar :: TC Type
newTyVar = do
  n <- get
  put (n + 1)
  return $ T_Var $ "a" ++ (show n)

instantiate :: TypeScheme -> TC Type
instantiate (TypeScheme vars t) = do
  nvars <- mapM (\_ -> newTyVar) vars
  let s = Map.fromList (zip vars nvars) in
    return $ subst (TypeSubst s) t

infer :: TypeEnv -> Expr -> TC (Type, TypeSubst)

-- literals

infer (TypeEnv env) (E_Lit (L_Int _))  = return (T_Basic "int", nullTypeSubst)
infer (TypeEnv env) (E_Lit (L_Bool _)) = return (T_Basic "bool", nullTypeSubst)
infer (TypeEnv env) (E_Lit L_Unit)     = return (T_Basic "unit", nullTypeSubst)

-- emptylist?

infer env (E_Lit L_EmptyList) = do
  t <- newTyVar
  return (T_List t, nullTypeSubst)

-- variable access

infer (TypeEnv env) (E_Access (Ident var)) = case Map.lookup var env of
  Nothing -> fail $ "Variable " ++ var ++ " doesn't have a type"
  Just scheme -> do
    t <- instantiate scheme
    return (t, nullTypeSubst)

infer env (E_Access (FieldAccess a field)) = do
  let (Just fieldOpType) = Map.lookup field (Map.fromList fieldOperatorTypes) in
    inferApp env fieldOpType [E_Access a] nullTypeSubst

-- operator expressions

infer env (E_BinOp op e1 e2) = do
  let (Just opType) = Map.lookup op (Map.fromList operatorTypes) in
    inferApp env opType [e1, e2] nullTypeSubst

infer env (E_UnOp op e1) = do
  let (Just opType) = Map.lookup op (Map.fromList operatorTypes) in
    inferApp env opType [e1] nullTypeSubst

-- function call

infer env (E_FunCall (FunCall id args)) = do
  (funType, s0) <- infer env (E_Access (Ident id))
  -- FunCall used to have a funExpr, now not any more...
  inferApp env funType args' s0
  where
    args' = case args of
      [] -> [E_Lit L_Unit]
      otherwise -> args

-- tuple expression

infer env (E_Tuple e1 e2) = do
  (t1, s1) <- infer env e1
  (t2, s2) <- infer (subst s1 env) e2
  return (T_Tuple (subst s2 t1) t2, s2 `composeTypeSubst` s1)

-- let expression

infer (TypeEnv env) (E_Let x e1 e2) = do
  (t1, s1) <- infer (TypeEnv env) e1
  let env'  = (Map.delete x env)
      t'    = generalize (subst s1 (TypeEnv env)) t1
      env'' = (TypeEnv (Map.insert x t' env')) in
        do
          (t2, s2) <- infer (subst s1 env'') e2
          return (t2, s1 `composeTypeSubst` s2)

-- generic application inference

inferApp :: TypeEnv -> Type -> [Expr] -> TypeSubst -> TC (Type, TypeSubst)
inferApp env0 funType args s0 = do
  ret_t <- newTyVar

  {-
    foldM :: Monad (...) =>
    (((Type, TypeSubst), TypeEnv, [Type], TypeSubst) -> Expr -> TC ((Type, TypeSubst), TypeEnv, [Type], TypeSubst)) ->
     ((Type, TypeSubst), TypeEnv, [Type], TypeSubst) ->
                                               [Expr] ->
                                                        TC ((Type, TypeSubst), TypeEnv, [Type], TypeSubst)
  -}
  ((tN, sN), envN, arg_types, full_sN) <- foldM
    (
      \((ti, si), env_i, arg_types, full_si) a_exp -> do
        (ti', si') <- infer (subst si env_i) a_exp
        return ((ti', si'), subst si env_i, ti' : arg_types, si' `composeTypeSubst` full_si)
    )
    ((funType, s0), env0, [], s0)
    (reverse args)

  let s_final = mgu funType (foldr T_Fun ret_t arg_types) in
    return (subst s_final ret_t, s_final `composeTypeSubst` full_sN)

