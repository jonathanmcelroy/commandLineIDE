{-# LANGUAGE DoAndIfThenElse #-}

module Latex where

import System.Directory
import System.FilePath.Find
import System.IO

import Functions

ideInitLatex :: IDEState -> IO (IDEState)
ideInitLatex state = do
    exists <- projectExists
    if exists then do
        putStrLn "Project already exists"
        return state
    else withFile ".project" WriteMode (\file -> do
            hPutStrLn file "Type: latex"
            latexFile <- getLatexFile
            hPutStrLn file $ "Files: " ++ show latexFile
            return state
        )

getLatexFile :: IO (Maybe FilePath)
getLatexFile = do
    files <- find always (extension ==? ".tex") =<< getCurrentDirectory
    return (if not (null files) then Just (head files) else Nothing)

