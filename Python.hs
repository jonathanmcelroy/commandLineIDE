{-# LANGUAGE DoAndIfThenElse #-}

module Python where

import System.Directory
import System.FilePath
import System.FilePath.Find
import System.IO

import Functions

-- initiate a haskell project
ideInitPython :: IDEState -> IO (IDEState)
ideInitPython state = do
    exists <- projectExists
    if exists then do
        putStrLn "Project already exists"
        return state
    else do
        thisDirectory <- getCurrentDirectory
        withFile ".project" WriteMode (\file -> do
            hPutStrLn file "Type: python"
            pythonFiles <- getRecursivePythonFiles
            hPutStrLn file $ "Files: " ++ show (map (makeRelative thisDirectory) pythonFiles)
            )
        return state

-- get all the haskell files in the current and subdirectories
getRecursivePythonFiles :: IO [FilePath]
getRecursivePythonFiles = find always (extension ==? ".py") =<< getCurrentDirectory

-- compile command for haskell
-- TODO: remove and use haskell.project file when I can get aeson
ideCompilePython :: IO ()
ideCompilePython = putStrLn "No need to compile python files"

-- make command for haskell
-- TODO: remove and use haskell.project file when I can get aeson
ideMakePython :: IO ()
ideMakePython = putStrLn "No need to compile python files"

getMainFilePython :: String -> [FilePath] -> FilePath
getMainFilePython _ _ = ""
