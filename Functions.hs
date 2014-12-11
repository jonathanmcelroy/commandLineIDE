module Functions where

import System.Directory
import System.FilePath.Find
import System.Process
import Control.Monad
import System.FilePath

import Languages

-- run a command from the command line
ideRunCommand :: String -> [String] -> IO ()
ideRunCommand command args = runProcess command args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()

-- .project file methods
projectExists :: IO Bool
projectExists = do
    dir <- getDirectoryContents =<< getCurrentDirectory
    return $ ".project" `elem` dir

getProjectAttributes :: IO (Maybe Language, [FilePath])
getProjectAttributes = do
    proj <- projectExists
    if proj then readFile ".project" >>= return . getAttributesFromLines . map words . lines else return (Nothing, [])

getAttributesFromLines :: [[String]] -> (Maybe Language, [FilePath])
getAttributesFromLines [] = (Nothing, [])
getAttributesFromLines (["Type:", projType] : others) = (getLanguage projType, snd $ getAttributesFromLines others)
getAttributesFromLines (["Files:", files] : others) = (fst (getAttributesFromLines others), read files)
getAttributesFromLines (_ : others) = getAttributesFromLines others

-- code file methods
getRecursiveCodeFiles :: IO [FilePath]
getRecursiveCodeFiles = do
    thisDirectory <- getCurrentDirectory
    liftM (map $ makeRelative thisDirectory) $ find always (
        extension ==? ".hs" ||?
        extension ==? ".py" ||?
        extension ==? ".tex"
        ) thisDirectory