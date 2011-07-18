{-# LANGUAGE DeriveDataTypeable #-}

module YLM.Runtime (Runtime(..), RuntimeException(..), run) where

import YLM.Elem
import YLM.TextInterface
import System.IO
import Data.Typeable
import qualified Control.Exception as E

class Runtime a where
  -- | Evaluate an expression purely with no side effects.
  evaluate :: a            -- ^ Which runtime to use.
           -> Elem         -- ^ The expression to evaluate.
           -> Elem         -- ^ The result of the expression. If the expression executes side effects,
                           -- 'evaluate' should return the furthest reduced form of those side effects.

  -- | Evaluate a list of instructions with IO (side effects).
  execute  :: a            -- ^ Which runtime to use.
           -> [Elem]       -- ^ The expressions to execute.
           -> IO (a, Elem) -- ^ The result of the last expression and progressed runtime with side effects evaluated.

  -- Default execution routine for a side-effect free language.
  execute r [] = return (r,Nil)
  execute r es = return (r,last $ map (evaluate r) es)

data RuntimeException = RuntimeException String deriving (Show,Typeable)
instance E.Exception RuntimeException

run :: (TextInterface a, Runtime a) => a -> String -> Either String (IO (a, Elem))
run r str = either Left (\ y -> Right $ execute r y) (ylmRead r str)