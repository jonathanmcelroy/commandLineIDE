{-# LANGUAGE DoAndIfThenElse #-}

module Haskell where

import Control.Monad
import System.Directory
import System.FilePath
import System.FilePath.Find
import System.IO

import Functions
import Languages
import Print

-- TODO: make this configurable
-- initiate a haskell project
ideInitHaskell :: IDEState -> IO (IDEState)
ideInitHaskell state@(Nothing, _) = do
    exists <- projectExists
    if exists then do
        printError "Project already exists"
        return state
    else do
        thisDirectory <- getCurrentDirectory
        files <- getDirectoryContents thisDirectory
        unless (any ((".cabal"==) . takeExtension) files) $ ideRunCommand "cabal" ["init"]
        haskFiles <- getRecursiveHaskellFiles
        withFile ".project" WriteMode $ \file -> do
            hPutStrLn file "Type: haskell"
            hPutStrLn file $ "Files: " ++ show (map (makeRelative thisDirectory) haskFiles)
        return (Just Haskell, haskFiles)
ideInitHaskell s = printError "Project already exists" >> return s

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
{-ideMakeHaskell = ideRunCommand "ghc" ["main.hs", "-Wall", "--make", "-odir", "obj", "-hidir", "obj", "-o", "out"]-}
ideMakeHaskell = ideRunCommand "cabal" ["install"]

getMainFileHaskell :: String -> [FilePath] -> FilePath
getMainFileHaskell _ _ = ""
