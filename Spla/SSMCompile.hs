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
  compile :: a -> Compiler Asm


type Asm = [String]


type Location = [String]


data CompileState = CompileState {
  -- fresh labels
    cNumLabels :: Int,

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
compileP p = unlines . beautifyAsm $ evalState (compile p) emptyCompileState


instance Compileable Program where
  compile (Program typedecls stmts) = do
    collectConstructors (foldl (\adts t -> case t of { TD_ADT adt -> adt:adts; _ -> adts }) [] typedecls)
    let mainCall = S_FunCall $ FunCall "main" [E_Lit L_Unit]
    cb <- compile (S_Block (builtinGlobalFunctions ++ stmts ++ [ mainCall ]))
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


--      [[ h ]] :: 0 -> k
-- ---------------------------
-- [[ match ... h ]] :: 0 -> k
instance Compileable a => Compileable (Match a) where
  compile m = return []


-- [[ s ]] :: 0 -> 0
instance Compileable Stmt where

  -- match
  compile (S_Let l) = compile l

  -- match
  compile (S_Match m) = compile m

  -- assembly
  -- HACKY
  compile (S_Asm asm) = return asm

  -- skip
  compile S_Skip = return $ [ "ajs 0 // skip" ]

  -- block
  compile (S_Block stmts) = do
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
      cstmts <- foldM (\asm stmt -> [ asm ++ cstmt | cstmt <- compile stmt ]) [] stmts

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
  compile (S_Declare _ id e) = compile (S_Assign (Ident id) e)

  -- assign, shallow
  compile (S_Assign (Ident id) e) = do
    currentLocation <- gets cLocation
    (varLocation, i) <- envLookup id
    ce <- compile e
    return $
      ce ++
      asm_lexical_store (length currentLocation - length varLocation) i

  -- assign, deep
  compile (S_Assign a e) =
    let (id, d:ds) = explodeAccess a in do
      currentLocation <- gets cLocation
      (varLocation, i) <- envLookup id
      ce <- compile e
      return $                                                          -- [0]
        ce ++                                                           -- [1] expression value
        asm_lexical_load (length currentLocation - length varLocation) i ++ -- [2] pointer to list/tuple
        concat (map compileOp ds) ++                                    -- [2] pointer to save location
        fieldStoreInstructions d                                        -- save (? TODO)

  -- function call
  compile (S_FunCall f) = compileFunCall f

  -- return
  compile (S_Return e) = do
    ce <- compile e
    return $
      ce ++
      [ "str RR" ] ++
      [ "bra __ret" ]

  -- if branch
  compile (S_If e b1 b2) = do
    ce <- compile e
    
    thenBody <- compile b1
    elseBody <- compile b2

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
  compile (S_While e b) = do
    ce <- compile e
    body <- compile b

    whileLabel <- freshLabel "_while"
    endLabel <- freshLabel "_end"

    return $
      [ whileLabel ++ ": ajs 0 // while" ] ++
      ce ++
      [ "brf " ++ endLabel ++ " // do" ] ++
      body ++
      [ "bra " ++ whileLabel,
        endLabel ++ ": ajs 0 // endwhile" ]


-- [[ e ]] :: 0 -> 1
instance Compileable Expr where

  -- let
  compile (E_Let l) = compile l

  -- match
  compile (E_Match m) = compile m

  -- literals
  compile (E_Lit (L_Int n))   = return $ [ "ldc " ++ show n ] -- TODO don't allow overflowing values
  compile (E_Lit (L_Bool b))  = return $ [ "ldc " ++ show (if b then machineTrue else machineFalse) ]
  compile (E_Lit L_Unit)      = return $ [ "ldc " ++ show machineUnit ]
  compile (E_Lit L_EmptyList) = return $ [ "ldc " ++ show machineEmptyList ]

  -- access
  compile (E_Access (Ident id)) = do
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

  compile (E_Access (FieldAccess a field)) = do
    c <- compile (E_Access a)
    return $
      c ++
      compileOp field

  -- function call
  compile (E_FunCall f) = do
    c <- compileFunCall f
    return $
      c ++
      [ "ldr RR" ]

  -- function expression, take 2
  compile (E_Fun params (S_Block stmts)) = do
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
      cstmts <- foldM (\asm stmt -> [ asm ++ cstmt | cstmt <- compile stmt ]) [] stmts

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

  compile (E_Fun params s) = do
    error $ "should not occur, block: " ++ (show s)

  -- operations
  compile (E_BinOp op e1 e2) = do
    ce1 <- compile e1
    ce2 <- compile e2
    return $ ce1 ++ ce2 ++ compileOp op
  compile (E_UnOp op e) = do
    ce <- compile e
    return $ ce ++ compileOp op

  compile (E_Tuple e1 e2) = do
    ce1 <- compile e1
    ce2 <- compile e2
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
  compile (Let x e h) = do
    oldLocation <- gets cLocation
    oldLocalsEnv <- gets cLocalsEnv

    letLocationLabel <- freshLabel "let"

    ce <- compile e

    let newLocation = oldLocation ++ [letLocationLabel]
      in do

      -- update location and environment
      modify $ \s -> s { cLocation = newLocation }
      modify $ \m -> m { cLocalsEnv = Map.insert x (newLocation, 3) oldLocalsEnv }

      -- compute body asm
      ch <- compile h

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


compileFunCall :: FunCall -> Compiler Asm
compileFunCall f@(FunCall id args) = do
  constructors <- gets cConstructors
  case Map.lookup id constructors of
    Just (adtSize, cNo) -> compileData adtSize cNo args
    Nothing -> compileRealFunCall f


compileData :: Int -> Int -> [Expr] -> Compiler Asm
compileData adtSize cNo args = do
  cargs <- foldM (\asm arg -> [ asm ++ carg | carg <- compile arg ]) [] args
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
    cid <- compile (E_Access (Ident id))
    cargs <- foldM (\asm arg -> [ asm ++ carg | carg <- compile arg ]) [] args

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