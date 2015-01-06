module Functions where

import Control.Monad (liftM)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (makeRelative)
import System.FilePath.Find (find, always, extension, (==?), (||?))
import System.Process (runProcess, waitForProcess)
import System.IO.Strict (readFile)

import Languages

-- run a command from the command line
ideRunCommand :: String -> [String] -> IO ()
ideRunCommand command args = runProcess command args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()

-------------------------------------------------------------------------------
-- IDE state
-------------------------------------------------------------------------------
type IDEState = (Maybe Language, [FilePath])


------------------------
-- .project file methods
------------------------

projectExists :: IO Bool
projectExists = doesFileExist ".project"

getProjectAttributes :: IO IDEState
getProjectAttributes = do
    proj <- projectExists
    if proj then do
        contents <- System.IO.Strict.readFile ".project"
        return $ getAttributesFromLines $ map words $ lines contents
    else return (Nothing, [])

getAttributesFromLines :: [[String]] -> IDEState
getAttributesFromLines [] = (Nothing, [])
getAttributesFromLines (["Type:", projType] : others) = (getLanguage projType, snd $ getAttributesFromLines others)
getAttributesFromLines (["Files:", files] : others) = (fst (getAttributesFromLines others), read files)
getAttributesFromLines (_ : others) = getAttributesFromLines others

-- code file methods
-- TODO: make this extentionable
getRecursiveCodeFiles :: IO [FilePath]
getRecursiveCodeFiles = do
    thisDirectory <- getCurrentDirectory
    liftM (map $ makeRelative thisDirectory) $ find always (
        extension ==? ".hs" ||?
        extension ==? ".py" ||?
        extension ==? ".tex"
        ) thisDirectory
