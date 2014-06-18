{-# LANGUAGE MonadComprehensions, TypeSynonymInstances, FlexibleInstances #-}

module Spla.Desugar where
  -- desugar :: AST_Program -> AST_Program



import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State

-- import qualified Text.PrettyPrint as PP
import Debug.Trace

import Utils

import Spla.Language


desugar :: AST_Program -> AST_Program
desugar p = p
