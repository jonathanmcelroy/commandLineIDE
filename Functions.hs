module Functions where

import Control.Monad
import System.Directory
import System.FilePath
import System.FilePath.Find
import System.Process

import Languages

-- run a command from the command line
ideRunCommand :: String -> [String] -> IO ()
ideRunCommand command args = runProcess command args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()

-------------------------------------------------------------------------------
-- IDE state
-------------------------------------------------------------------------------
type IDEState = (Maybe Language, [FilePath])


-- .project file methods
projectExists :: IO Bool
projectExists = doesFileExist ".project"

getProjectAttributes :: IO IDEState
getProjectAttributes = do
    proj <- projectExists
    if proj then readFile ".project" >>= return . getAttributesFromLines . map words . lines else return (Nothing, [])

getAttributesFromLines :: [[String]] -> IDEState
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
