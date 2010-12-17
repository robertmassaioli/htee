module Main (main) where

{-
 - This is a Version of Tee written in Haskell for the Cross Platform and parallel benefits
 -}

import System.IO
import System.Environment ( getArgs )
import System.Console.GetOpt

import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
      (flags, [],      [])     -> handleFlags flags
      (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
      (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Help

header :: String
header = "Usage: htee [options] files..."

options :: [OptDescr Flag] 
options = [ 
  Option ['V','v'] ["version"] (NoArg Version) "print program version number",
  Option ['H','h'] ["help"] (NoArg Help) "print this help message"
  ]

handleFlags :: [Flag] -> IO ()
handleFlags _ = runTee

-- The actual code that runs the tee operation
runTee :: IO ()
runTee = do
  is_eof <- isEOF
  if is_eof 
    then return ()
    else do
      getLine >>= putStrLn >> runTee
