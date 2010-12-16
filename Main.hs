module Main (main) where

{-
 - This is a Version of Tee written in Haskell for the Cross Platform and parallel benefits
 -}

import System.IO
--import System( getArgs )
import System.Console.GetOpt

import Control.Monad

main = runTee

runTee :: IO ()
runTee = do
  is_eof <- isEOF
  if is_eof 
    then return ()
    else do
      getLine >>= putStrLn >> runTee
