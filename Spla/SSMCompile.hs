{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, FlexibleInstances #-}

module Spla.SSMCompile where
  -- compileP :: Program -> String

-- TODO garbage collection


import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Char (isSpace)
import Text.Regex.Posix

import Control.Monad.State

-- import qualified Text.PrettyPrint as PP
import Debug.Trace

import Utils

import Spla.Language

machineTrue :: Int
machineTrue = 0xFFFFFFFF

machineFalse :: Int
machineFalse = 0

machineUnit :: Int
machineUnit = 0

machineEmptyList :: Int
machineEmptyList = 0


class Compileable a where
  -- first argument: alternative compilation scheme flag
  compile :: String -> a -> Compiler Asm


type Asm = [String]


type Location = [String]


data CompileState = CompileState {
  -- fresh labels
    cNumLabels :: Int,

  -- match end labels
    cMatchEndLabels :: [String],

  -- data types
    -- maps constructor names to adt size and adt c #
    cConstructors :: Map.Map String (Int, Int),

  -- handling lexical scopes
    cLocation :: Location,
    cLocalsEnv :: Map.Map String (Location, Int),

  -- collect function code
    cFunctionCode :: [Asm]
  }
  deriving (Show, Eq)

emptyCompileState :: CompileState
emptyCompileState = CompileState {
    cNumLabels = 0,

    cMatchEndLabels = [],

    cConstructors = Map.empty,

    cLocation = [],
    cLocalsEnv = Map.empty,

    cFunctionCode = []
  }


type Compiler a = State CompileState a


envLookup :: String -> Compiler (Location, Int)
envLookup x = do
  env <- gets cLocalsEnv
  case Map.lookup x env of
    Nothing -> error $ "unknown identifier: " ++ x -- should never happen
    Just e  -> return e


freshLabel :: String -> Compiler String
freshLabel prefix = do
  i <- gets cNumLabels
  modify $ \m -> m { cNumLabels = i + 1 }
  return $ prefix ++ show i


addFunctionCode :: Asm -> Compiler ()
addFunctionCode asm = do
  existingFunctionCode <- gets cFunctionCode
  modify $ \s -> s { cFunctionCode = asm : existingFunctionCode }


compileP :: Program -> String
compileP p = unlines . beautifyAsm $ evalState (compile [] p) emptyCompileState


instance Compileable Program where
  compile flag (Program typedecls stmts) = do
    collectConstructors (foldl (\adts t -> case t of { TD_ADT adt -> adt:adts; _ -> adts }) [] typedecls)
    let mainCall = S_FunCall $ FunCall "main" [E_Lit L_Unit]
    cb <- compile flag (S_Block (builtinGlobalFunctions ++ stmts ++ [ mainCall ]))
    functionsCode <- gets cFunctionCode
    return $
      [ "ldc 0 -- DUMMY",
        "str MP" ] ++
      cb ++
      [ "ldr RR",
        "trap 0",
        "halt" ] ++
      concat functionsCode ++
      builtinAsm


collectConstructors :: [ADT] -> Compiler ()
collectConstructors [] = return ()
collectConstructors (ADT _ _ cs : adts) = do
  let adtSize = foldl max 0 (map (\(Constructor _ args) -> length args) cs)
  let newConstructors = Map.fromList $ mapZI (\i (Constructor name _) -> (name, (adtSize, i))) cs
  currentConstructors <- gets cConstructors
  modify $ \m -> m { cConstructors = currentConstructors `Map.union` newConstructors }
  collectConstructors adts


instance Compileable a => Compileable [a] where
  compile flag hs = foldM (\asm h -> [ asm ++ ch | ch <- compile flag h ]) [] hs


--      [[ h ]] :: 0 -> k
-- ---------------------------
-- [[ match ... h ]] :: 0 -> k
instance Compileable a => Compileable (Match a) where
  compile flag (Match e rules) = do
    ce <- compile flag e
    newEndLabel <- freshLabel "_m_end"
    oldEndLabels <- gets cMatchEndLabels
    modify $ \m -> m { cMatchEndLabels = newEndLabel : oldEndLabels }
    crules <- compile flag rules
    modify $ \m -> m { cMatchEndLabels = oldEndLabels }
    return $
      ce ++
      [ "str R5" ] ++ -- TODO change this to allow for nested match constructs
      crules ++
      [ newEndLabel ++ ": ajs 0" ]


-- TODO create local context
instance Compileable a => Compileable (MatchRule a) where
  compile flag (MatchRule e to) = do
    oldLocation <- gets cLocation
    oldLocalsEnv <- gets cLocalsEnv

    continueLabel <- freshLabel "_m"
    (endLabel : _) <- gets cMatchEndLabels

    matchRuleLocationLabel <- freshLabel "mrule"

    let newLocation = oldLocation ++ [matchRuleLocationLabel]
        newLocals = Set.elems (fv e)
        newLocalsEnv = Map.fromList $ zip newLocals $ mapI (\i id -> (newLocation, i + 2)) newLocals
      in do

      -- update location and environment
      modify $ \s -> s { cLocation = newLocation }
      modify $ \m -> m { cLocalsEnv = newLocalsEnv `Map.union` oldLocalsEnv }

      -- compute body asm
      cme <- compile "match_expr" e
      ceq <- compile "match_check" e
      cto <- compile flag to

      -- restore location and environment
      modify $ \s -> s { cLocation = oldLocation }
      modify $ \m -> m { cLocalsEnv = oldLocalsEnv }

      return $
        [ "ldc 0" ] ++                          -- [1] 0. (not a function)
        [ "ldr MP" ] ++                         -- [2] 1. parent context
        [ "ldr MP" ] ++                         -- [3] 2. return context = parent context
        replicate (length newLocals) "ldc 0" ++
        asm_enter_ctxt (3 + length newLocals) "lexical match rule" ++
        cme ++
        [ "ldr R5" ] ++
        ceq ++
        [ "brf " ++ continueLabel ] ++
        cto ++
        [ "bra " ++ endLabel,
          continueLabel ++ ": ajs 0" ] ++
        asm_exit_ctxt


-- [[ s ]] :: 0 -> 0
instance Compileable Stmt where

  -- match
  compile flag (S_Let l) = compile flag l

  -- match
  compile flag (S_Match m) = compile flag m

  -- assembly
  -- HACKY
  compile flag (S_Asm asm) = return asm

  -- skip
  compile flag S_Skip = return $ [ "ajs 0 // skip" ]

  -- block
  compile flag (S_Block stmts) = do
    oldLocation <- gets cLocation
    oldLocalsEnv <- gets cLocalsEnv

    blockLocationLabel <- freshLabel "block"

    let newLocation = oldLocation ++ [blockLocationLabel]
        newLocals = blockVars (S_Block stmts)
        newLocalsEnv = Map.fromList $ zip newLocals $ mapI (\i id -> (newLocation, i + 2)) newLocals
      in do

      -- update location and environment
      modify $ \s -> s { cLocation = newLocation }
      modify $ \m -> m { cLocalsEnv = newLocalsEnv `Map.union` oldLocalsEnv }

      -- compute body asm
      cstmts <- compile flag stmts

      -- restore location and environment
      modify $ \s -> s { cLocation = oldLocation }
      modify $ \m -> m { cLocalsEnv = oldLocalsEnv }

      return $
        [ "ldc 0" ] ++                          -- [1] 0. (not a function)
        [ "ldr MP" ] ++                         -- [2] 1. parent context
        [ "ldr MP" ] ++                         -- [3] 2. return context = parent context
        replicate (length newLocals) "ldc 0" ++
        asm_enter_ctxt (3 + length newLocals) "lexical block" ++
        cstmts ++
        asm_exit_ctxt

  -- declare
  compile flag (S_Declare _ id e) = compile flag (S_Assign (Ident id) e)

  -- assign, shallow
  compile flag (S_Assign (Ident id) e) = do
    currentLocation <- gets cLocation
    (varLocation, i) <- envLookup id
    ce <- compile flag e
    return $
      ce ++
      asm_lexical_store (length currentLocation - length varLocation) i

  -- assign, deep
  compile flag (S_Assign a e) =
    let (id, d:ds) = explodeAccess a in do
      currentLocation <- gets cLocation
      (varLocation, i) <- envLookup id
      ce <- compile flag e
      return $                                                          -- [0]
        ce ++                                                           -- [1] expression value
        asm_lexical_load (length currentLocation - length varLocation) i ++ -- [2] pointer to list/tuple
        concat (map compileOp ds) ++                                    -- [2] pointer to save location
        fieldStoreInstructions d                                        -- save (? TODO)

  -- function call
  compile flag (S_FunCall f) = compile flag f

  -- return
  compile flag (S_Return e) = do
    ce <- compile flag e
    return $
      ce ++
      [ "str RR" ] ++
      [ "bra __ret" ]

  -- if branch
  compile flag (S_If e b1 b2) = do
    ce <- compile flag e
    
    thenBody <- compile flag b1
    elseBody <- compile flag b2

    elseLabel <- freshLabel "_else"
    endLabel <- freshLabel "_end"

    return $
      [ "ajs 0 // if" ] ++
      ce ++
      [ "brf " ++ elseLabel ++ " // then" ] ++
      thenBody ++
      [ "bra " ++ endLabel ] ++
      [ elseLabel ++ ": ajs 0 // else:" ] ++
      elseBody ++
      [ endLabel ++ ": ajs 0 // endif" ]

  -- while branch
  compile flag (S_While e b) = do
    ce <- compile flag e
    body <- compile flag b

    whileLabel <- freshLabel "_while"
    endLabel <- freshLabel "_end"

    return $
      [ whileLabel ++ ": ajs 0 // while" ] ++
      ce ++
      [ "brf " ++ endLabel ++ " // do" ] ++
      body ++
      [ "bra " ++ whileLabel,
        endLabel ++ ": ajs 0 // endwhile" ]


-- [[ l ]] :: 0 -> 1
instance Compileable Lit where
  compile flag (L_Int n)     = return $ [ "ldc " ++ show n ] -- TODO don't allow overflowing values
  compile flag (L_Bool b)    = return $ [ "ldc " ++ show (if b then machineTrue else machineFalse) ]
  compile flag (L_Unit)      = return $ [ "ldc " ++ show machineUnit ]
  compile flag (L_EmptyList) = return $ [ "ldc " ++ show machineEmptyList ]


-- [[ e ]] :: 0 -> 1
instance Compileable Expr where

  -- Match Expression alternative compilation schemes
  ---------------------------------------------------
  compile "match_expr" (E_Access (Ident id)) = return [ "ldc 0" ]
  compile "match_expr" (E_MatchWildcard)     = return [ "ldc 0" ]


  -- Match Expression checkins alternative compilation schemes :: 2 -> 1
  ----------------------------------------------------------------------
  compile "match_check" (E_MatchWildcard) = return [ "ajs -2", "ldc " ++ show machineTrue ]
  compile "match_check" (E_Lit l)         = return [ "eq" ]
  {-
  -- TODO
  -- check name first,
  --  then all args
  --  don't forget to short circuit!
  compile "match_check" (E_Data cName args) = do
    constructors <- gets cConstructors
    let (Just k) = Map.lookup cName constructors
    return $
      concat (mapZI chArgEq args) ++
      replicate (length args - 1) "and" ++
      [ "ajs -2",
        "lds 2" ]
    where
      chArgEq i e =
        [ "lds -" ++ show (i + 1),
          "ldh " ++ show (i + 1),
          "lds -" ++ show (i + 1),
          "ldh " ++ show (i + 1) ] ++
        compile "match_check" cons cap e
  -}
  compile "match_check" (E_Tuple e1 e2) = do
    ceq1 <- compile "match_check" e1
    ceq2 <- compile "match_check" e2
    return $
      [ "lds -1" ] ++
      compileOp "fst" ++
      [ "lds -1" ] ++
      compileOp "fst" ++
      ceq1 ++
      [ "lds -2" ] ++
      compileOp "snd" ++
      [ "lds -2" ] ++
      compileOp "snd" ++
      ceq2 ++
      [ "and",
        "ajs -3",
        "lds 3" ]
  compile "match_check" (E_BinOp ":" e1 e2) = do
    ceq1 <- compile "match_check" e1
    ceq2 <- compile "match_check" e2
    return $
      [ "lds -1" ] ++
      compileOp "hd" ++
      [ "lds -1" ] ++
      compileOp "hd" ++
      ceq1 ++
      [ "lds -2" ] ++
      compileOp "tl" ++
      [ "lds -2" ] ++
      compileOp "tl" ++
      ceq2 ++
      [ "and",
        "ajs -3",
        "lds 3" ]
  compile "match_check" (E_Access (Ident x)) = do
    (_, i) <- envLookup x
    return $
      asm_lexical_store 0 i ++
      [ "ajs -1",
        "ldc " ++ show machineTrue ]


  -- Normal Expressions
  ---------------------

  -- let
  compile flag (E_Let l) = compile flag l

  -- match
  compile flag (E_Match m) = compile flag m

  -- literals
  compile flag (E_Lit l) = compile flag l

  -- access
  compile flag (E_Access (Ident id)) = do
    constructors <- gets cConstructors
    case Map.lookup id constructors of
      Just (adtSize, cNo) -> do
        cd <- compileData adtSize cNo []
        return $
          cd ++
          [ "ldr RR" ]
      Nothing -> do
        currentLocation <- gets cLocation
        (location, i) <- envLookup id
        return $ asm_lexical_load (length currentLocation - length location) i

  compile flag (E_Access (FieldAccess a field)) = do
    c <- compile flag (E_Access a)
    return $
      c ++
      compileOp field

  -- function call
  compile flag (E_FunCall f) = do
    c <- compile flag f
    return $
      c ++
      [ "ldr RR" ]

  -- function expression, take 2
  compile flag (E_Fun params (S_Block stmts)) = do
    oldLocation <- gets cLocation
    oldLocalsEnv <- gets cLocalsEnv

    functionLabel <- freshLabel "fun"

    let newLocation = oldLocation ++ [functionLabel]
        blockLocals = blockVars (S_Block stmts)
        newLocals = params ++ blockLocals
        newLocalsEnv = Map.fromList $ zip newLocals $ mapI (\i x -> (newLocation, i + 2)) newLocals
      in do

      -- update location and environment
      modify $ \s -> s { cLocation = newLocation }
      modify $ \m -> m { cLocalsEnv = newLocalsEnv `Map.union` oldLocalsEnv }

      -- compute body asm
      cstmts <- compile flag stmts

      -- restore location and environment
      modify $ \s -> s { cLocation = oldLocation }
      modify $ \m -> m { cLocalsEnv = oldLocalsEnv }

      -- store code
      addFunctionCode $
        [ "",
          "__" ++ functionLabel ++ ": ldr PC",
          "ldc 6",
          "add",
          "str RR",
          "ret",
          "",
          "_" ++ functionLabel ++ ": ajs 0",
          "ldc 1",                                                          -- [1] 0. function context flag
          "lds " ++ show (- 2 - length params),                             -- [2] 1. parent context (defining context)
          "ldr MP" ] ++                                                     -- [3] 2. return context
        replicate (length params) ("lds " ++ show (- 3 - length params)) ++ --        formal arguments
        replicate (length blockLocals) "ldc 0" ++                           --        block locals
        asm_enter_ctxt (3 + length newLocals) "lexical fun" ++
        cstmts
        --asm_exit_ctxt ++  -- this is not really needed, as every function
        --[ "bra __ret" ]   --  must have a return statement

      -- return pointer to function object
      return $
        [ "bsr __" ++ functionLabel,
          "ldr RR", -- [1] 0. pointer to function code
          "ldr MP", -- [2] 1. defining lexical context
          "stmh 2", -- [1]
          "annote HP -2 -2 gray \"fun object\"",
          "ldc 1",  -- [2]
          "sub" ]   -- [1] pointer to function object

  compile flag (E_Fun params s) = do
    error $ "should not occur, block: " ++ (show s)

  -- operations
  compile flag (E_BinOp op e1 e2) = do
    ce1 <- compile flag e1
    ce2 <- compile flag e2
    return $ ce1 ++ ce2 ++ compileOp op
  compile flag (E_UnOp op e) = do
    ce <- compile flag e
    return $ ce ++ compileOp op

  compile flag (E_Tuple e1 e2) = do
    ce1 <- compile flag e1
    ce2 <- compile flag e2
    return $
      ce1 ++
      ce2 ++
      [ "ajs -1 // start save tuple",
        "sth",
        "ajs 1",
        "sth",
        "ajs -1 // end save tuple" ]


--       [[ h ]] :: 0 -> k
-- ----------------------------
-- [[ let ... in h ]] :: 0 -> k
instance Compileable a => Compileable (Let a) where
  compile flag (Let x e h) = do
    oldLocation <- gets cLocation
    oldLocalsEnv <- gets cLocalsEnv

    letLocationLabel <- freshLabel "let"

    ce <- compile flag e

    let newLocation = oldLocation ++ [letLocationLabel]
      in do

      -- update location and environment
      modify $ \s -> s { cLocation = newLocation }
      modify $ \m -> m { cLocalsEnv = Map.insert x (newLocation, 3) oldLocalsEnv }

      -- compute body asm
      ch <- compile flag h

      -- restore location and environment
      modify $ \s -> s { cLocation = oldLocation }
      modify $ \m -> m { cLocalsEnv = oldLocalsEnv }

      return $
        [ "ldc 0" ] ++                          -- [1] 0. (not a function)
        [ "ldr MP" ] ++                         -- [2] 1. parent context
        [ "ldr MP" ] ++                         -- [3] 2. return context = parent context
        ce ++
        asm_enter_ctxt 4 "lexical let" ++
        ch ++
        asm_exit_ctxt


instance Compileable FunCall where
  compile flag f@(FunCall id args) = do
    constructors <- gets cConstructors
    case Map.lookup id constructors of
      Just (adtSize, cNo) -> compileData adtSize cNo args
      Nothing -> compileRealFunCall f


compileData :: Int -> Int -> [Expr] -> Compiler Asm
compileData adtSize cNo args = do
  cargs <- compile "" args
  return $
    [ "ldc " ++ show cNo ++ " // save data" ] ++
    cargs ++
    (replicate (adtSize - length args) "ldc 0") ++
    asm_heapify (adtSize + 1) ++
    [ "str RR" ]


compileRealFunCall :: FunCall -> Compiler Asm
compileRealFunCall f@(FunCall id args_) =
  let args = (if length args_ == 0 then [E_Lit L_Unit] else args_)
      n = length args
    in do
    cid <- compile "" (E_Access (Ident id))
    cargs <- compile "" args

    return $                      -- [0]
      cid ++                      -- [1]             pointer to function object
      [ "lda 1" ] ++              -- [1]     0.      pointer to function defining context (to be pulled in by function)
      cargs ++                    -- [n + 1] 1 .. n. formal arguments                     (to be pulled in by function)
      cid ++                      -- [n + 2]         pointer to function object
      [ "lda 0" ] ++              -- [n + 2]         pointer to function code
      [ "jsr" ] ++                -- [n + 1]
      [ "ajs -" ++ show (n + 1) ] -- [0]             // clean up stack



-- asm_heapify n :: n -> 1
asm_heapify :: Int -> Asm
asm_heapify n =
  [ "stmh " ++ show n,
    "ldc " ++ show (n - 1),
    "sub" ]

-- asm_enter_ctxt n :: n -> 0
asm_enter_ctxt :: Int -> String -> Asm
asm_enter_ctxt n annotation = -- annotation = "lexical block" | "lexical let" | "lexical fn"
  [ "stmh " ++ show n,
    "annote HP -" ++ show n ++ " -" ++ show n ++ " gray \"" ++ annotation ++ "\"",
    "ldc " ++ show (n - 1),
    "sub",
    "str MP" ]

-- asm_exit_ctxt :: 0 -> 0
asm_exit_ctxt :: Asm
asm_exit_ctxt =
  [ "ldr MP",
    "lda 2", -- return context
    "str MP" ]

-- asm_lexical_load up i :: 0 -> 1
asm_lexical_load :: Int -> Int -> Asm
asm_lexical_load up i =
  [ "ldr MP" ] ++
  replicate up "lda 1" ++ -- parent context
  [ "lda " ++ show i ]

-- asm_lexical_store up i :: 1 -> 0
asm_lexical_store :: Int -> Int -> Asm
asm_lexical_store up i =
  [ "ldr MP" ] ++
  replicate up "lda 1" ++ -- parent context
  [ "sta " ++ show i ]



compileOp :: String -> Asm

compileOp "*"  = [ "mul" ]
compileOp "/"  = [ "div" ]
compileOp "%"  = [ "mod" ]
compileOp "+"  = [ "add" ]
compileOp "-"  = [ "sub" ]

compileOp "~"  = [ "neg" ]

compileOp "<=" = [ "le" ]
compileOp ">=" = [ "ge" ]
compileOp "==" = [ "eq" ]
compileOp "!=" = [ "ne" ]
compileOp ">"  = [ "gt" ]
compileOp "<"  = [ "lt" ]

compileOp "&&" = [ "and" ]
compileOp "||" = [ "or" ]

compileOp "!"  = [ "not" ]

compileOp ":"  = [ "ajs -1 // start perform cons",
                   "sth",
                   "ajs 1",
                   "sth",
                   "ajs -1 // end perform cons" ]

compileOp "fst" = [ "ldh 0 // access fst" ]
compileOp "snd" = [ "ldh 1 // access snd" ]

compileOp "hd"  = [ "lds 0 // check list non-emptyness",
                    "ldc 0",
                    "eq",
                    "brt _EXCEPTION_EmptyList",
                    "ldh 0 // access hd" ]
compileOp "tl"  = [ "lds 0 // check list non-emptyness",
                    "ldc 0",
                    "eq",
                    "brt _EXCEPTION_EmptyList",
                    "ldh 1 // access tl" ]



fieldStoreInstructions :: String -> Asm

-- No check needed for initializedness of tuple,
--  because [compile (S_Assign (FieldAccess a field) e)]
--  first evaluates [compile (E_Access a)], where
--  the check is already done.
fieldStoreInstructions "fst"  = [ "sta 0 // start save fst",
                                  "lds 2 // end save fst" ]
fieldStoreInstructions "snd"  = [ "sta 1 // start save snd",
                                  "lds 2 // end save snd" ]

fieldStoreInstructions "hd"   = [ "lds 0 // check list non-emptyness",
                                  "ldc 0",
                                  "eq",
                                  "brt _EXCEPTION_EmptyList",
                                  "sta 0 // start save hd",
                                  "lds 2 // end save hd" ]
fieldStoreInstructions "tl"   = [ "lds 0 // check list non-emptyness",
                                  "ldc 0",
                                  "eq",
                                  "brt _EXCEPTION_EmptyList",
                                  "sta 1 // start save tl",
                                  "lds 2 // end save tl" ]



builtinGlobalFunctions :: [Stmt]
builtinGlobalFunctions = [
    S_Declare (typeFun (T_Var "a") (T_Concrete "unit"))
      "print"
      (E_Fun ["x"] (S_Block [S_Asm $
          [ "ldr MP",
            "lda 3",
            "trap 0",
            "ldc 0",
            "str RR" ] ++
          asm_exit_ctxt ++
          [ "ret" ]
        ])),
    S_Declare (typeFun (T_Var "a") (T_Concrete "unit"))
      "printchr"
      (E_Fun ["x"] (S_Block [S_Asm $
          [ "ldr MP",
            "lda 3",
            "trapchr 0",
            "ldc 0",
            "str RR" ] ++
          asm_exit_ctxt ++
          [ "ret" ]
        ])),
    S_Declare (typeFun (typeList (T_Var "a")) (T_Concrete "bool"))
      "isEmpty"
      (E_Fun ["x"] (S_Block [S_Asm $
          [ "ldr MP",
            "lda 3",
            "ldc 0",
            "eq",
            "str RR" ] ++
          asm_exit_ctxt ++
          [ "ret" ]
        ]))
  ]



builtinAsm :: Asm
builtinAsm =
  [ "",
    "// ====== BUILTINS ====== //",
    "",
    "__ret:       ldr MP",             -- [1] MP
    "             lda 0",              -- [1] function?
    "             brf __ret_cont" ] ++ -- [0]
                  asm_exit_ctxt ++
  [ "             ret",
    "__ret_cont:  ldr MP",             -- [1] MP
    "             lda 1",              -- [1] parent MP
    "             str MP",             -- [0]
    "             bra __ret",
    "",
    "_EXCEPTION_EmptyList:      ajs 0",
    "                           annote SP 0 0 red \"EXCEPTION empty list\"",
    "                           trap 0",
    "                           halt" ]



padStr :: Int -> String -> String
padStr n str = str ++ (replicate (n - length str) ' ')

beautifyAsm :: [String] -> [String]
beautifyAsm lines = map beautifyAsmLine lineBits
  where
    lineBits = map explode lines

    explode line =
      let (_, _, _, label : instr : comment : _) = (line =~ "^([^:]*:)?[ ]*([^/]*)(//.*)?$" :: (String, String, String, [String]))
        in [trim  label, trim  instr, trim  comment]

    labelColLen = min 10 $ max $ map (length . head) lineBits
    instrColLen = min 10 $ max $ map (length . head . tail) lineBits

    max :: [Int] -> Int
    max list = (reverse $ sort list) !! 0

    trim :: String -> String
    trim = f . f
       where f = reverse . dropWhile isSpace

    beautifyAsmLine [label, instr, comment] = (padStr labelColLen label) ++ " " ++ (padStr instrColLen instr) ++ " " ++ comment