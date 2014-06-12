{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, FlexibleInstances #-}

{-
  "Mapping High-Level Constructs to LLVM IR"
  http://llvm.lyngvig.org/Articles/Mapping-High-Level-Constructs-to-LLVM-IR
  - ? data layout
  - ? target triple
  - locals on stack -> "memory to register promotion" opt. pass
  - ? structures: packed or not
-}

module Splb.LLVMCompile where
  -- compile :: Program -> String

import Splb.Language

compile :: Program -> String
compile p = ""

{-

Types
=====

  integer     = i32 ...
  label
  first class = integer {structure ...} ...
  derived     = function, structure ...


Type mapping
============

  m(unit)       =
  m(int)        = i32
  m(bool)       =
  m([ t ])      = structure?
  m(( t1, t2 )) = { m(t1), m(t2) }

  "Linked lists"
  https://gist.github.com/jeremytregunna/1144415

    %free_func = type void (i8*)* ; pointer to function that takes an i8 pointer and returns nothing
    %list = type { %list*, i8*, %free_func }
    ...
    ret %list* ...


Functions
=========

  define i32 @MyFunction(i32 %a, i32 %b) nounwind {
    ret i32 42
  }


Instructions
============

  %1 = call i32 @MyFunction(i32 4, i32 5)
  ret i32 42

-}