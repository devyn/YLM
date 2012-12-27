{-# LANGUAGE GADTs #-}

module YLM.Repl (StateDisplay(..), repl) where

import YLM.Data
import YLM.Runtime
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
--import System.Console.Readline

data StateDisplay i where
  StateDisplay :: (TextInterface i, PrettyPrint i) => (i -> Scope -> (String, StateDisplay i)) -> StateDisplay i

repl :: (TextInterface i, PrettyPrint i)
     => i              -- ^ The text interface/pretty printer to use
     -> Scope          -- ^ The initial scope to seed the interpreter with
     -> StateDisplay i -- ^ Used to generate the prompt
     -> IO ()

repl i m (StateDisplay f) = do
  let (p, f') = f i m
  --ms <- readline $ concat ["\x01\ESC[1m\x02", p, ">>\x01\ESC[0m\x02 "]
  putStr $ concat ["\x01\ESC[1m\x02", p, ">>\x01\ESC[0m\x02 "]
  hFlush stdout
  ms <- getLine >>= \ s ->
    case s of
         "" -> return Nothing
         _  -> return (Just s)
  case ms of
    Nothing -> repl i m f'
    Just s  -> --do
      --addHistory s
      case yread i "(interactive)" s of
        Left err -> do
          putStrLn $ concat [" \ESC[0;31mERROR: ", err, "\ESC[0m"]
          hFlush stdout
          repl i m f'
        Right ie ->
          case ie of
            [Label "quit"] -> return ()
            _ -> do
              mre <- yexec m ie
              case mre of
                Left err -> do
                  putStrLn $ concat [" \ESC[0;31mERROR: ", err, "\ESC[0m"]
                  hFlush stdout
                  repl i m f'
                Right (m', r) ->
                  do putStrLn $ " " ++ (ypp i m' 1 [r])
                     hFlush stdout
                     repl i m' f'
