{-# LANGUAGE DeriveDataTypeable #-}

module YLM.Runtime (Runtime(..), run) where

import YLM.Elem
import YLM.TextInterface
import System.IO
import Data.Typeable
import Data.Either
import Control.Monad.Trans.Error

class Runtime a where
  -- | Evaluate an expression purely with no side effects.
  evaluate :: a                            -- ^ Which runtime to use.
           -> Elem                         -- ^ The expression to evaluate.
           -> Either String Elem           -- ^ Either an error or the result of the expression. If the expression executes side
                                           -- effects, 'evaluate' should return the furthest reduced form of those side effects.

  -- | Evaluate a list of instructions with IO (side effects).
  execute  :: a                            -- ^ Which runtime to use.
           -> [Elem]                       -- ^ The expressions to execute.
           -> IO (Either String (a, Elem)) -- ^ Either an error, or the result of the last expression and progressed runtime
                                           -- with side effects evaluated.

  -- Default execution routine for a side-effect free language.
  execute r [] = return $ Right $ (r,Nil)
  execute r es = return $ mapM (evaluate r) es >>= \ xs -> Right (r,last xs)

run :: (TextInterface a, Runtime a) => a -> String -> IO (Either String (a, Elem))
run r str = either (return . Left) (execute r) (ylmRead r str)