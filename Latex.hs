{-# LANGUAGE DoAndIfThenElse #-}

module Latex where

import System.Directory
import System.FilePath.Find
import System.IO

import Functions

ideInitLatex :: IO()
ideInitLatex = do
    exists <- projectExists
    if exists then
        putStrLn "Project already exists"
    else withFile ".project" WriteMode (\file -> do
            hPutStrLn file "Type: latex"
            latexFile <- getLatexFile
            hPutStrLn file $ "Files: " ++ show latexFile
            )

getLatexFile :: IO (Maybe FilePath)
getLatexFile = do
    files <- find always (extension ==? ".tex") =<< getCurrentDirectory
    return (if not (null files) then Just (head files) else Nothing)

