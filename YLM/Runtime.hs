{-# LANGUAGE DeriveDataTypeable #-}

module YLM.Runtime (Runtime(..), RuntimeException(..), run, repl) where

import YLM.Elem
import YLM.TextInterface
import System.IO
import System.Console.Readline
import Data.Typeable
import qualified Control.Exception as E

class Runtime a where
  -- | Evaluate an expression purely with no side effects.
  evaluate :: a       -- ^ Which runtime to use.
           -> Elem    -- ^ The expression to evaluate.
           -> Elem    -- ^ The result of the expression. If the expression executes side effects,
                      -- 'evaluate' should return the furthest reduced form of those side effects.

  -- | Evaluate a list of instructions with IO (side effects).
  execute  :: a       -- ^ Which runtime to use.
           -> [Elem]  -- ^ The expressions to execute.
           -> IO Elem -- ^ The result of the last expression with side effects evaluated.

  -- Default execution routine for a side-effect free language.
  execute r [] = return Nil
  execute r es = return . last $ map (evaluate r) es

data RuntimeException = RuntimeException String deriving (Show,Typeable)
instance E.Exception RuntimeException

run :: (TextInterface a, Runtime a) => a -> String -> Either String (IO Elem)
run r str = either Left (\ y -> Right $ execute r y) (ylmRead r str)

repl :: (Show a, TextInterface a, Runtime a) => a -> IO ()
repl r = do
  ms <- readline $ (show r) ++ ">> "
  case ms of
    Nothing -> repl r
    Just s  -> do
      addHistory s
      case ylmRead r s of
        Left err -> do
          putStrLn $ (show r) ++ "!> ERROR: " ++ err
          repl r
        Right ie ->
          case ie of
            [Label "quit"] -> return ()
            _ -> do
              mre <- E.catch (Right `fmap` execute r ie)
                     (return . Left . (show :: E.SomeException -> [Char]))
              case mre of
                Left err -> do
                  putStrLn $ (show r) ++ "!> ERROR: " ++ err
                  repl r
                Right re ->
                  case ylmWrite r [re] of
                    Left err -> do
                      putStrLn $ (show r) ++ "!> ERROR: " ++ err
                      repl r
                    Right os -> do
                      putStrLn $ (show r) ++ "=> " ++ os
                      repl r