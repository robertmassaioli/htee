module Main (main) where

{-
 - This is a remake of 'tee' written in Haskell for the Cross Platform benefits.
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
  case getOpt Permute options args of
      (flags, nonOpts, [])     -> handleFlags flags nonOpts
      (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Help | Append
          deriving(Eq)

header :: String
header = "Usage: htee [options] output_files..."

versionInfo :: String
versionInfo = "htee-0.1 by Robert Massaioli (2010)"

options :: [OptDescr Flag] 
options = [ 
  Option "Vv" ["version"] (NoArg Version) "print program version number",
  Option "Hh" ["help"] (NoArg Help) "print this help message",
  Option "a" ["append"] (NoArg Append) "append to the files, do not truncate them"
  ]

handleFlags :: [Flag] -> [String] -> IO ()
handleFlags flags possibleFilenames
  | Help `elem` flags = putStrLn (usageInfo header options)
  | Version `elem` flags = putStrLn versionInfo
  | otherwise = do 
      validHandles <- validFilePaths possibleFilenames >>= mapM (`openFile` selectFileMode flags)
      runTee validHandles
      mapM_ hClose validHandles
  where
    selectFileMode :: [Flag] -> IOMode
    selectFileMode flags = if Append `elem` flags then AppendMode else WriteMode

    runTee :: [Handle] -> IO ()
    runTee handles = go
      where 
        go :: IO ()
        go = do
          is_eof <- isEOF
          unless is_eof $ do
              line <- getLine 
              mapM_ (`hPutStrLn` line) handles 
              putStrLn line 
              go

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
        validFilePath = doesDirectoryExist . emptyToCurrent . dropFileName
          where
            emptyToCurrent :: FilePath -> FilePath
            emptyToCurrent "" = "./"
            emptyToCurrent x  = x
