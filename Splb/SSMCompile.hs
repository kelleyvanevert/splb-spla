{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, FlexibleInstances #-}

module Splb.SSMCompile where
  -- compile :: Program -> String

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

import Splb.Language

machineTrue :: Int
machineTrue = 0xFFFFFFFF

machineFalse :: Int
machineFalse = 0

machineUnit :: Int
machineUnit = 0

machineEmptyList :: Int
machineEmptyList = 0


type Asm = [String]


data CompileState = CompileState {
    cNumLabels :: Int,

    cLocalsEnv :: Map.Map String Int,

    cCurrentFnName :: String,
    cCurrentFnArgs :: [String],
    cCurrentFnLocals :: [String]
  }
  deriving (Show, Eq)

emptyCompileState :: CompileState
emptyCompileState = CompileState {
    cNumLabels = 0,

    cLocalsEnv = Map.empty,

    cCurrentFnName = "",
    cCurrentFnArgs = [],
    cCurrentFnLocals = []
  }


type Compiler a = State CompileState a


envLookup :: String -> Compiler Int
envLookup id = do
  env <- gets cLocalsEnv
  case Map.lookup id env of
    Nothing -> error $ "unknown identifier: " ++ id -- should never happen
    Just e  -> return e


freshLabel :: Compiler String
freshLabel = do
  name <- gets cCurrentFnName
  i <- gets cNumLabels
  modify $ \m -> m { cNumLabels = i + 1 }
  return $ "_" ++ name ++ "_l" ++ show i


compile :: Program -> String
compile p = unlines . beautifyAsm $ evalState (compileProgram p) emptyCompileState


compileProgram :: Program -> Compiler Asm
compileProgram (Program fundecls) = do
  cfundecls <- foldM
                  (\asm fundecl -> [ asm ++ cfundecl | cfundecl <- compileFunDecl fundecl ])
                  []
                  fundecls
  return $
    [ "link 0 // enter program",
      "ldc 0 // unit arg for main()",
      "bsr _main // call main()",
      "ldr RR",
      "trap 0",
      "halt" ] ++
    cfundecls ++
    builtins


compileFunDecl :: FunDecl -> Compiler Asm
compileFunDecl (FunDecl _ id params b) =
  let argNames = map (\(Param id _) -> id) params
      -- numArgs = max 1 (length argNames)
  in do
    modify $ \s -> s { cCurrentFnName = id }
    modify $ \s -> s { cCurrentFnArgs = argNames }
    modify $ \s -> s { cCurrentFnLocals = [] }
    modify $ \s -> s { cLocalsEnv = Map.fromList $
                          zip (map (\(Param id _) -> id) params)
                              (map (\i -> - 2 - (length params) + i) [1 .. (length params)])
                        }

    body <- compileBlock b -- compile body, count total number of locals (including let's)
    locals <- gets cCurrentFnLocals

    return $
      [ "",
        "_" ++ id ++ ": link " ++ show (2 * length locals) ++ " // enter fn, allocate locals" ] ++
      (mapI (\i id -> "annote MP " ++ show (2*i-1) ++ " " ++ show (2*i-1) ++ " orange \"local " ++ id ++ "\"") locals) ++
      (mapI (\i id -> "annote MP " ++ show (2*i  ) ++ " " ++ show (2*i  ) ++ " orange \"init? local " ++ id ++ "\"") locals) ++
      body


{- Compiling a block:
   # add number of locals to fn number of locals (for fn [link] instruction)
   # modify locals env to point to correct locals
     # recursively compile stmts
   # reset locals env
-}
compileBlock :: Block -> Compiler Asm
compileBlock (Block vardecls stmts) = do
  oldLocals <- gets cCurrentFnLocals
  modify $ \s -> s { cCurrentFnLocals = oldLocals ++ map (\(VarDecl id _) -> id) vardecls }

  oldLocalsEnv <- gets cLocalsEnv
  
  modify $ \m -> m { cLocalsEnv = foldl
                        (\env (i, (VarDecl id _)) -> Map.insert id (2 * length oldLocals + i) env)
                        oldLocalsEnv
                        (zip [1,3 .. (2 * length vardecls)] vardecls)
                      }

  cstmts <- foldM (\asm stmt -> [ asm ++ cstmt | cstmt <- compileStmt stmt ]) [] stmts

  modify $ \m -> m { cLocalsEnv = oldLocalsEnv }
  return cstmts


compileStmt :: Stmt -> Compiler Asm

-- skip
compileStmt S_Skip = return $ [ "ajs 0 // skip" ]

-- block
compileStmt (S_Block b) = compileBlock b

-- return
compileStmt (S_Return Nothing) = compileStmt (S_Return (Just (E_Lit L_Unit)))
compileStmt (S_Return (Just e)) = do
  ce <- compileExpr e
  return $
    ce ++
    [ "str RR",
      "unlink",
      "ret" ]

-- function call
compileStmt (S_FunCall fc) = compileFunCall fc

-- assign
-- identifier assign is a simple by-value assign
compileStmt (S_Assign (Ident id) e) = do
  ce <- compileExpr e
  localLoc <- envLookup id
  return $
    ce ++
    [ "stl " ++ show localLoc,
      "ldc 1",
      "stl " ++ show (localLoc + 1) ]

-- field access assign is a by-ref assign, where we use all but the
--  last field access to determine the heap location, and then the last
--  field access translates into storage instructions
compileStmt (S_Assign (FieldAccess a field) e) = do
  ce <- compileExpr e
  ca <- compileExpr (E_Access a) -- will be a heap pointer
  return $
    ce ++
    ca ++
    fieldStoreInstructions field

-- if branch
compileStmt (S_If e b1 mb2) = do
  ce <- compileExpr e
  
  thenBody <- compileBlock b1
  elseBody <- compileStmt $ case mb2 of
                               Nothing -> S_Skip
                               Just b2 -> S_Block b2

  elseLabel <- freshLabel
  endLabel <- freshLabel

  return $
    [ "ajs 0 // if" ] ++
    ce ++
    [ "brf " ++ elseLabel ++ " // then" ] ++
    thenBody ++
    [ "bra " ++ endLabel ] ++
    [ elseLabel ++ ": ajs 0 // else:" {- skip stm -} ] ++
    elseBody ++
    [ endLabel ++ ": ajs 0 // endif" ]

-- while branch
compileStmt (S_While e b) = do
  ce <- compileExpr e
  body <- compileBlock b

  whileLabel <- freshLabel
  endLabel <- freshLabel

  return $
    [ whileLabel ++ ": ajs 0 // while" ] ++
    ce ++
    [ "brf " ++ endLabel ++ " // do" ] ++
    body ++
    [ "bra " ++ whileLabel,
      endLabel ++ ": ajs 0 // endwhile" ]


compileExpr :: Expr -> Compiler Asm

-- literals
compileExpr (E_Lit (L_Int n))   = return $ [ "ldc " ++ show n ] -- TODO don't allow overflowing values
compileExpr (E_Lit (L_Bool b))  = return $ [ "ldc " ++ show (if b then machineTrue else machineFalse) ]
compileExpr (E_Lit L_Unit)      = return $ [ "ldc " ++ show machineUnit ]
compileExpr (E_Lit L_EmptyList) = return $ [ "ldc " ++ show machineEmptyList ]

-- operations
compileExpr (E_BinOp op e1 e2) = do
  ce1 <- compileExpr e1
  ce2 <- compileExpr e2
  return $ ce1 ++ ce2 ++ compileOp op
compileExpr (E_UnOp op e) = do
  ce <- compileExpr e
  return $ ce ++ compileOp op

-- calling a function
compileExpr (E_FunCall fc) = do
  c <- compileFunCall fc
  return $ c ++ [ "ldr RR" ]

-- access
compileExpr (E_Access (Ident id)) = do
  localLoc <- envLookup id
  return $
    [ "ldl " ++ show (localLoc + 1),
      "brf _EXCEPTION_UninitiatedVar",
      "ldl " ++ show localLoc ]

compileExpr (E_Access (FieldAccess a field)) = do
  c <- compileExpr (E_Access a)
  return $
    c ++
    compileOp field

compileExpr (E_Tuple e1 e2) = do
  ce1 <- compileExpr e1
  ce2 <- compileExpr e2
  return $
    ce1 ++
    ce2 ++
    [ "ajs -1 // start save tuple",
      "sth",
      "ajs 1",
      "sth",
      "ajs -1 // end save tuple" ]

compileExpr (E_Let id e1 e2) = do
  locals <- gets cCurrentFnLocals
  modify $ \s -> s { cCurrentFnLocals = locals ++ ["let " ++ id] }
  let letVarLoc = 1 + 2 * length locals in do

    ce1 <- compileExpr e1

    oldLocalsEnv <- gets cLocalsEnv
    modify $ \m -> m { cLocalsEnv = Map.insert id letVarLoc oldLocalsEnv }

    ce2 <- compileExpr e2

    modify $ \m -> m { cLocalsEnv = oldLocalsEnv }

    return $
      ce1 ++
      [ "stl " ++ show letVarLoc,
        "ldc 1",
        "stl " ++ show (letVarLoc + 1) ] ++
      ce2


compileFunCall :: FunCall -> Compiler Asm
compileFunCall (FunCall f args_) =
  let args = (if length args_ == 0 then [E_Lit L_Unit] else args_) in do
    cargs <- foldM (\asm arg -> [ asm ++ carg | carg <- compileExpr arg ]) [] args
    return $
      cargs ++
      [ "bsr _" ++ f,
        "ajs " ++ show (- (length args)) ]


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
--  because [compileStmt (S_Assign (FieldAccess a field) e)]
--  first evaluates [compileExpr (E_Access a)], where
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


builtins :: Asm
builtins = [
    "",
    "// ====== BUILTINS ====== //",
    "",
    "_print:   link 0",
    "          ldl -2",
    "          trap 0",
    "          ajs 1",
    "          str RR",
    "          unlink",
    "          ret",
    "",
    "_isEmpty: link 0",
    "          ldl -2",
    "          ldc 0",
    "          eq",
    "          str RR",
    "          unlink",
    "          ret",
    "",
    "_EXCEPTION_EmptyList:      annote SP 0 0 red \"EXCEPTION empty list\"",
    "                           trap 0",
    "                           halt",
    "_EXCEPTION_UninitiatedVar: annote SP 0 0 red \"EXCEPTION uninitialized var\"",
    "                           trap 0",
    "                           halt"
  ]


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