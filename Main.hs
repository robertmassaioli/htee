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
      (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Help | Append
          deriving(Eq)

header :: String
header = "Usage: htee [options] files..."

options :: [OptDescr Flag] 
options = [ 
  Option "Vv" ["version"] (NoArg Version) "print program version number",
  Option "Hh" ["help"] (NoArg Help) "print this help message",
  Option "a" [] (NoArg Append) "append to the files, do not truncate them"
  ]

handleFlags :: [Flag] -> [String] -> IO ()
handleFlags flags possibleFilenames = do
  validFiles <- validFilePaths possibleFilenames 
  validHandles <- mapM (`openFile` selectFileMode flags) validFiles
  runTee validHandles
  mapM_ hClose validHandles
  where
    selectFileMode :: [Flag] -> IOMode
    selectFileMode flags = if Append `elem` flags
                              then AppendMode
                              else WriteMode

-- The actual code that runs the tee operation
runTee :: [Handle] -> IO ()
runTee handles = do
  is_eof <- isEOF
  unless is_eof $ do
      line <- getLine 
      mapM_ (`hPutStrLn` line) handles 
      putStrLn line 
      runTee handles

validFilePaths :: [FilePath] -> IO [FilePath]
validFilePaths [] = return []
validFilePaths (x:xs) = do
  result <- validFilePaths xs
  fileValid <- validFilePath x
  if fileValid 
    then return (x : result)
    else return result
  where
    validFilePath :: FilePath -> IO Bool
    validFilePath = doesDirectoryExist . dropFileName
