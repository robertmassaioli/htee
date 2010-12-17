module Main (main) where

{-
 - This is a Version of Tee written in Haskell for the Cross Platform and parallel benefits
 -}

import System.IO
import System.Environment ( getArgs )
import System.Console.GetOpt
import System.FilePath
import System.Directory

import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
      (flags, nonOpts,      [])     -> handleFlags flags nonOpts
      --(_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
      (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Help 

header :: String
header = "Usage: htee [options] files..."

options :: [OptDescr Flag] 
options = [ 
  Option ['V','v'] ["version"] (NoArg Version) "print program version number",
  Option ['H','h'] ["help"] (NoArg Help) "print this help message"
  ]

handleFlags :: [Flag] -> [String] -> IO ()
handleFlags _ possibleFilenames = do
  validFilePaths possibleFilenames >>= (\s -> putStrLn $ "Files are valid: " ++ show s)
  runTee

-- The actual code that runs the tee operation
runTee :: IO ()
runTee = do
  is_eof <- isEOF
  if is_eof 
    then return ()
    else do
      getLine >>= putStrLn >> runTee

validFilePaths :: [FilePath] -> IO Bool
validFilePaths [] = return True
validFilePaths (x:xs) = do
  fileValid <- validFilePath x
  if fileValid 
    then validFilePaths xs
    else return False
  where
    validFilePath :: FilePath -> IO Bool
    validFilePath = doesDirectoryExist . dropFileName
