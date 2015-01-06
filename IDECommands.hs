module IDECommands (ideCommand, ideCommands) where

import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import qualified Data.Map as M

import Functions
import Languages
import Print

import Haskell
import Latex
import Python

-- TODO: actually use state so that I can change the list of files during a run

-- the mapping from command names to functions
ideCommands :: M.Map String ([String] -> IDEState -> IO (IDEState))
ideCommands = M.fromList [
        ("help",    ideHelp),
        ("init",    ideInitLanguageString),
        ("list",    ideList),
        ("tag",     ideTag),
        ("make",    ideMake),
        ("edit",    ideEdit),
        ("run",     ideRun),
        ("delete",  ideDeleteProject)
    ]


-------------------------------------------------------------------------------
-- IDE functions
-------------------------------------------------------------------------------

-- call a function based on the given command and arguments
ideCommand :: [String] -> IDEState -> IO (IDEState)
ideCommand (command:args) state =
    let maybeValue = M.lookup command ideCommands
    in case maybeValue of
        Nothing -> printError "Unrecongnised command" >> return state
        Just value -> value args state
ideCommand [] state = return state

-------------------------
-- print the help massage
-------------------------
ideHelp :: [String] -> IDEState -> IO (IDEState)
ideHelp _ state = putStrLn (unlines [
        "Usage:",
        "   help:   outputs this help list",
        "   init:   initialized a project",
        "   list:   lists the files in a project",
        "   tag:    creates tags file",
        "   make:   compiles the project",
        "   edit:   opens the file in an editor",
        "   run:    runs the project's executable",
        "   delete: deletes the project (keeps the files)"
    ]) >> return state

-----------------------
-- initialize a project
-----------------------

-- initialize a project based on a language name
ideInitLanguageString :: [String] -> IDEState -> IO (IDEState)
ideInitLanguageString _ state@(Just _, _) = printError "Project already exists" >> return state
ideInitLanguageString (language : _) state = ideInitLanguage (getLanguage language) state
ideInitLanguageString [] state = ideInit state

-- initialize a project based on the files in the directory and subdirectories
ideInit :: IDEState -> IO (IDEState)
ideInit state = do
    files <- getRecursiveCodeFiles
    let language = getFilesLanguage files in case language of
        Just _ -> ideInitLanguage language state
        Nothing -> putStrLn "No single language" >> return state

-- initialize a project based on a language
ideInitLanguage :: Maybe Language -> IDEState -> IO (IDEState)
ideInitLanguage (Just Haskell) state = ideInitHaskell state
ideInitLanguage (Just Latex) state = ideInitLatex state
ideInitLanguage (Just Python) state = ideInitPython state
ideInitLanguage _ state = printError "Unsupported language" >> return state


--------------------------
-- list files in a project
--------------------------

-- TODO: should we also look for files in the directory?
-- list all files in a project
ideList :: [String] -> IDEState -> IO (IDEState)
ideList ["all"] state@(language, _) = case language of
    Just Haskell -> do
        files <- getRecursiveHaskellFiles
        putStr $ unlines files
        return state
    Just Python -> do
        files <- getRecursivePythonFiles
        putStr $ unlines files
        return state
    _ -> return state
ideList _ state@(_, files) = putStr (unlines files) >> return state

--------------------------------------
-- Perform tag operations in a project
--------------------------------------

-- TODO: generalize to all project types
-- tag files in a project
ideTag :: [String] -> IDEState -> IO(IDEState)
ideTag _ state = ideRunCommand "hasktags"
    ["--ignore-close-implementation", "--ctags", "."] >> return state


-------------------------
-- make/compile a project
-------------------------

-- TODO: we should do this using a makefile

-- make the project based on language
ideMake :: [String] -> IDEState -> IO(IDEState)
ideMake _ state = do
    (projectType, _) <- getProjectAttributes
    case projectType of
        Just Haskell -> ideMakeHaskell
        Just Python -> ideMakePython
        _ -> return ()
    return state


---------------------------
-- edit a file in a project
---------------------------

-- TODO: this should be configurable or use a default editor

-- open a file using nvim
ideEdit :: [String] -> IDEState -> IO(IDEState)
ideEdit []        state = putStrLn "You must specify a file to open" >> return state
ideEdit filenames state = ideRunCommand "nvim" filenames >> return state

-----------------------------
-- run the project executable
-----------------------------

-- TODO: we should split this into each language's file
-- run the project
ideRun :: [String] -> IDEState -> IO (IDEState)
ideRun _ state = do
    (projectType, _) <- getProjectAttributes
    case projectType of
        Just Haskell -> ideRunCommand "./out" []
        Just Python -> ideRunCommand "python" ["main.py"]
        _ -> putStrLn "Language does not support running"
    return state


-------------------
-- delete a project
-------------------

-- TODO: checking .project exists twice?
-- delete the .project file
ideDeleteProject :: [String] -> IDEState -> IO (IDEState)
ideDeleteProject _ state = do
    projExists <- projectExists
    when projExists $ do
        setRed
        putStrLn "Deleting .project file"
        exists <- doesFileExist ".project"
        when exists $ removeFile ".project"
        colorReset
    return state
