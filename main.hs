{-# LANGUAGE DoAndIfThenElse #-}

import Control.Monad (liftM, void, when)
import Control.Monad.State.Strict (lift)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Console.ANSI (Color(Green, Red), ColorIntensity(Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)
import System.Console.Haskeline (Completion, InputT, Settings, completeWordWithPrev, defaultSettings, getInputLine, runInputT, setComplete, simpleCompletion)
import System.Directory (doesFileExist, removeFile)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.IO (hFlush, stdout)
import System.FilePath ()
import qualified Data.Map as M

import Functions
import Languages

import Haskell
import Latex
import Python

-- TODO: use state to store the state of the project instead of rereading the
--       .project file
-- TODO: use aeson and json for .project file

-------------------------------------------------------------------------------
-- Main loop
-------------------------------------------------------------------------------
main :: IO ()
main = do
    project <- getProjectAttributes
    let loop :: InputT IO ()
        loop = do
            lift setRed
            lift printPrompt
            minput <- getInputLine ""
            case liftM commandSubstitute minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just input -> do
                    lift (ideCommand (words input) project)
                    loop
    runInputT hlSettings loop


-------------------------------------------------------------------------------
-- Command line
-------------------------------------------------------------------------------

-- The settings to use for the shell
hlSettings :: Settings IO
hlSettings = setComplete (completeWordWithPrev Nothing " \t" findCompletion)
    defaultSettings


-- Complete command line commands
findCompletion :: String -> String -> IO [Completion]
-- TODO: only look for tracked files and maybe all source files in directory
-- tree
-- match "edit "
findCompletion " tide" s = do
    dir <- getDirectoryContents =<< getCurrentDirectory
    return $ map simpleCompletion $ filter (s `isPrefixOf`) dir
-- match on commands
findCompletion _ s = return $ map simpleCompletion $ filter (s `isPrefixOf`)
    (M.keys ideCommands)


-- the mapping from command names to functions
ideCommands :: M.Map String ([String] -> IDEState -> IO ())
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
ideCommand :: [String] -> IDEState -> IO ()
ideCommand (command: args) state =
    let maybeValue = M.lookup command ideCommands
    in case maybeValue of
        Nothing -> printError "Unrecongnised command"
        Just value -> value args state
ideCommand [] _ = return ()

-------------------------
-- print the help massage
-------------------------
ideHelp :: [String] -> IDEState -> IO ()
ideHelp _ _ = putStrLn $ unlines [
        "Usage:",
        "   help:   outputs this help list",
        "   init:   initialized a project",
        "   list:   lists the files in a project",
        "   tag:    creates tags file",
        "   make:   compiles the project",
        "   edit:   opens the file in an editor",
        "   run:    runs the project's executable",
        "   delete: deletes the project (keeps the files)"
    ]

-----------------------
-- initialize a project
-----------------------

-- initialize a project based on a language name
ideInitLanguageString :: [String] -> IDEState -> IO ()
ideInitLanguageString _ (Just _, _) = printError "Project already exists"
ideInitLanguageString (language : _) _ = ideInitLanguage $ getLanguage language
ideInitLanguageString [] _ = ideInit

-- initialize a project based on the files in the directory and subdirectories
ideInit :: IO()
ideInit = do
    files <- getRecursiveCodeFiles
    let language = getFilesLanguage files in case language of
        Just _ -> ideInitLanguage language
        Nothing -> putStrLn "No single language"

-- initialize a project based on a language
ideInitLanguage :: Maybe Language -> IO ()
ideInitLanguage (Just Haskell) = ideInitHaskell
ideInitLanguage (Just Latex) = ideInitLatex
ideInitLanguage (Just Python) = ideInitPython
ideInitLanguage _ = printError "Unsupported language"


--------------------------
-- list files in a project
--------------------------

-- TODO: should we also look for files in the directory?
-- list all files in a project
ideList :: [String] -> IDEState -> IO()
ideList _ (projjectType, files) = do
    (projectType, _) <- getProjectAttributes
    void $ case projectType of
        Just Haskell -> getRecursiveHaskellFiles >>= putStr . unlines
        Just Python -> getRecursivePythonFiles >>= putStr . unlines
        _ -> getRecursiveCodeFiles >>= putStr . unlines


--------------------------------------
-- Perform tag operations in a project
--------------------------------------

-- tag files in a project
-- TODO: generalize to all project types
ideTag :: [String] -> IDEState -> IO()
ideTag _ _ = ideRunCommand "hasktags"
    ["--ignore-close-implementation", "--ctags", "."]


-------------------------
-- make/compile a project
-------------------------

-- TODO: we should do this using a makefile

-- make the project based on language
ideMake :: [String] -> IDEState -> IO()
ideMake _ _ = do
    (projectType, _) <- getProjectAttributes
    case projectType of
        Just Haskell -> ideMakeHaskell
        Just Python -> ideCompilePython
        _ -> return ()


---------------------------
-- edit a file in a project
---------------------------

-- TODO: this should be configurable or use a default editor

-- open a file using nvim
ideEdit :: [String] -> IDEState -> IO()
ideEdit []        _ = putStrLn "You must specify a file to open"
ideEdit filenames _ = ideRunCommand "nvim" filenames

-----------------------------
-- run the project executable
-----------------------------

-- TODO: we should split this into each language's file
-- run the project
ideRun :: [String] -> IDEState -> IO()
ideRun _ _ = do
    (projectType, _) <- getProjectAttributes
    case projectType of
        Just Haskell -> ideRunCommand "./out" []
        Just Python -> ideRunCommand "python" ["main.py"]
        _ -> putStrLn "Language does not support running"


-------------------
-- delete a project
-------------------

-- TODO: checking .project exists twice?
-- delete the .project file
ideDeleteProject :: [String] -> IDEState -> IO ()
ideDeleteProject _ _ = do
    projExists <- projectExists
    when projExists ( do
        setRed
        putStrLn "Deleting .project file"
        exists <- doesFileExist ".project"
        when exists $ removeFile ".project"
        colorReset
        )

------------------------
-- miscelaneous commangs
------------------------

-- substitute an equivalent command string using the default command name
commandSubstitute :: String -> String
commandSubstitute s = result
    where
        wList = words s
        result
            | null wList                                   = s
            | map toLower (head wList) `elem` exitCommands = "exit"
            | otherwise                                    = unwords
                (wordSubstitute (map toLower $ head wList) : tail wList)

-- substitute an equivalent command name with the default name
wordSubstitute :: String -> String
wordSubstitute "ls" = "list"
wordSubstitute s = s

-- the list of exist commands
exitCommands :: [String]
exitCommands = ["exit", ":q", "quit"]

-- print an error message
printError :: String -> IO ()
printError err = setRed >> putStrLn err >> colorReset

-- print the prompt
printPrompt :: IO ()
printPrompt = do
    setSGR [SetColor Foreground Vivid Green]
    proj <- projectExists
    if proj then do
        (Just projType, _) <- getProjectAttributes
        putStr (show projType ++ " > ")
    else
        putStr "no project > "
    colorReset >> hFlush stdout

--------------
-- Color codes
--------------

-- set the color to red
setRed :: IO ()
setRed = setSGR [SetColor Foreground Vivid Red]

-- reset the color
colorReset :: IO ()
colorReset = setSGR [Reset]
