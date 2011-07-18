module YLM.Repl (repl) where

import YLM.Elem
import YLM.TextInterface
import YLM.Runtime
import YLM.PrettyPrint
import System.IO
import System.Console.Readline
import qualified Control.Exception as E

repl :: (Show a, TextInterface a, Runtime a, PrettyPrint a) => a -> IO ()
repl r = do
  ms <- readline $ "\x01\ESC[1m\x02" ++ (show r) ++ ">>\x01\ESC[0m\x02 "
  case ms of
    Nothing -> repl r
    Just s  -> do
      addHistory s
      case ylmRead r s of
        Left err -> do
          putStrLn $ "\ESC[1m" ++ (show r) ++ "!> \ESC[0;31mERROR: " ++ err ++ "\ESC[0m"
          repl r
        Right ie ->
          case ie of
            [Label "quit"] -> return ()
            _ -> do
              mre <- E.catch (Right `fmap` execute r ie)
                     (return . Left . (show :: E.SomeException -> [Char]))
              case mre of
                Left err -> do
                  putStrLn $ "\ESC[1m" ++ (show r) ++ "!> \ESC[0;31mERROR: " ++ err ++ "\ESC[0m"
                  repl r
                Right (nr, re) ->
                  case ylmPrettyPrint nr (length (show nr) + 3) [re] of
                    Left err -> do
                      putStrLn $ "\ESC[1m" ++ (show r) ++ "!> \ESC[0;31mERROR: " ++ err ++ "\ESC[0m"
                      repl r
                    Right os -> do
                      putStrLn $ "\ESC[1m" ++ (show nr) ++ "=>\ESC[0m " ++ os
                      repl nr