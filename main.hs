{-# LANGUAGE DoAndIfThenElse #-}

import Control.Monad (liftM)
import Control.Monad.State.Strict (lift)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.Console.Haskeline (Completion, InputT, Settings, completeWordWithPrev, defaultSettings, getInputLine, runInputT, setComplete, simpleCompletion)
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.FilePath ()
import qualified Data.Map as M

import Functions (getProjectAttributes, IDEState)
import IDECommands (ideCommand, ideCommands)
import Print (setRed, printPrompt)

-- TODO: use aeson and json for .project file
-- TODO: autocomplete "init" on language names

-------------------------------------------------------------------------------
-- Main loop
-------------------------------------------------------------------------------
main :: IO ()
main = do
    firstState <- getProjectAttributes
    let loop :: IDEState -> InputT IO ()
        loop state = do
            -- set the color to red
            lift setRed
            lift $ printPrompt state
            minput <- getInputLine ""
            case liftM commandSubstitute minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just input -> do
                    newState <- lift (ideCommand (words input) state)
                    loop newState
    runInputT hlSettings $ loop firstState


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
exitCommands = ["exit", ":q", "quit", ":quit"]

