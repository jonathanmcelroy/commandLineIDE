{-# LANGUAGE DoAndIfThenElse #-}

module Haskell where

import Control.Monad
import System.Directory
import System.FilePath
import System.FilePath.Find
import System.IO

import Functions

-- initiate a haskell project
ideInitHaskell :: IO ()
ideInitHaskell = do
    exists <- projectExists
    if exists then
        putStrLn "Project already exists"
    else do
        thisDirectory <- getCurrentDirectory
        files <- getDirectoryContents thisDirectory
        unless (any ((".cabal"==) . takeExtension) files) $ ideRunCommand "cabal" ["init"]
        withFile ".project" WriteMode (\file -> do
            hPutStrLn file "Type: haskell"
            haskFiles <- getRecursiveHaskellFiles
            hPutStrLn file $ "Files: " ++ show (map (makeRelative thisDirectory) haskFiles)
            )

-- get all the haskell files in the current and subdirectories
getRecursiveHaskellFiles :: IO [FilePath]
getRecursiveHaskellFiles = getCurrentDirectory >>= (\thisDirectory -> liftM (map $ makeRelative thisDirectory) $ find always (extension ==? ".hs") thisDirectory)

-- compile command for haskell
-- TODO: remove and use haskell.project file when I can get aeson
{-ideCompileHaskell :: IO ()-}
{-ideCompileHaskell = ideRunCommand "ghc" ["main.hs", "-odir", "obj", "-hidir", "obj", "-o", "out"]-}

-- make command for haskell
-- TODO: remove and use haskell.project file when I can get aeson
ideMakeHaskell :: IO ()
ideMakeHaskell = ideRunCommand "ghc" ["main.hs", "-Wall", "--make", "-odir", "obj", "-hidir", "obj", "-o", "out"]

getMainFileHaskell :: String -> [FilePath] -> FilePath
getMainFileHaskell _ _ = ""
