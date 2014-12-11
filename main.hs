{-# LANGUAGE DoAndIfThenElse #-}

import Control.Monad
import System.Process (runCommand, waitForProcess)
import Data.Char
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.IO
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import qualified Data.Map as M

import Functions
import Languages

import Haskell
import Latex
import Python

{-main :: IO ((), Namespace)-}
{-main = runStateT (runInputT hlSettings repl) []-}
{-[>main = do<]-}
    {-[>setRed<]-}
    {-[>printPrompt<]-}
    {-[>s <- getContents<]-}
    {-[>_ <- mapM ide $ takeWhile (`notElem` exitCommands) $ lines s<]-}
    {-[>colorReset<]-}

findCompletion :: String -> IO [Completion]
findCompletion s = return $ map simpleCompletion $ filter (s `isPrefixOf`) (M.keys ideCommands)

hlSettings :: Settings IO
hlSettings = setComplete (completeWord Nothing " \t" findCompletion) defaultSettings

main :: IO ()
main = runInputT hlSettings loop
    where
        loop :: InputT IO ()
        loop = do
            lift setRed
            lift printPrompt
            minput <- getInputLine ""
            case liftM commandSubstitute minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just input -> do lift (ideCommand (words input))
                                 loop


-- This will take cin split by lines and perform IO from the commands

ideCommands :: M.Map String ([String] -> IO ())
ideCommands = M.fromList [
        ("help", ideHelp),
        ("init", ideInitLanguageString),
        ("list", ideList),
        ("tag", \_ -> return ()),
        ("make", ideMake),
        ("edit", ideEdit),
        ("run", ideRun),
        ("delete", ideDeleteProject)
    ]

ideCommand :: [String] -> IO ()
ideCommand (command: args) = let maybeValue = M.lookup command ideCommands in case maybeValue of
    Nothing -> printError "Unrecongnised command"
    Just value -> value args
ideCommand [] = return ()

ideHelp :: [String] -> IO ()
ideHelp _ = putStrLn $ unlines [
        "Usage:",
        "   help:   outputs this help list",
        "   init:   initialized a project",
        "   list:   lists the files in a project",
--         "   tag:    does stuff with tags",
        "   make:   compiles the project",
        "   edit:   opens the file in an editor",
        "   run:    runs the project's executable",
        "   delete: deletes the project (keeps the files"
    ]

{-ideCommand [] = return ()-}
{-ideCommand ["init"] = ideInit-}
{-ideCommand ["init", language] = ideInitLanguage $ getLanguage language-}
{-ideCommand ["ls"] = ideList-}
{-ideCommand ["list"] = ideList-}
{-ideCommand ["tag"] = return ()-}
{-ideCommand ["compile"] = ideCompile-}
{-ideCommand ["make"] = ideMake-}
{-ideCommand ["edit"] = return ()-}
{-ideCommand ["run"] = ideRun-}
{-ideCommand ["delete", "project"] = ideDeleteProject-}
{-ideCommand ["\\"] = return ()-}
{-ideCommand ("\\":command:args) = ideRunCommand command args-}
{-ideCommand _ = printError "Unrecongnised command"-}


-- initiate a project
ideInitLanguageString :: [String] -> IO ()
ideInitLanguageString (language : _) = ideInitLanguage $ getLanguage language
ideInitLanguageString [] = ideInit

ideInit :: IO()
ideInit = do
    files <- getRecursiveCodeFiles
    let language = getFilesLanguage files in case language of
        Just _ -> ideInitLanguage language
        Nothing -> putStrLn "No single language"

ideInitLanguage :: Maybe Language -> IO ()
ideInitLanguage (Just Haskell) = ideInitHaskell
ideInitLanguage (Just Latex) = ideInitLatex
ideInitLanguage (Just Python) = ideInitPython
ideInitLanguage _ = printError "Unsupported language"


-- list files in a project
ideList :: [String] -> IO()
ideList _ = do
    (projectType, _) <- getProjectAttributes
    void $ case projectType of
        Just Haskell -> getRecursiveHaskellFiles >>= putStr . unlines
        Just Python -> getRecursivePythonFiles >>= putStr . unlines
        _ -> getRecursiveCodeFiles >>= putStr . unlines

-- make a project
ideMake :: [String] -> IO()
ideMake _ = do
    (projectType, _) <- getProjectAttributes
    case projectType of
        Just Haskell -> ideMakeHaskell
        Just Python -> ideCompilePython
        _ -> return ()

-- edit a file
ideEdit :: [String] -> IO()
ideEdit [] = putStrLn "You must specify a file to open"
ideEdit filenames = do
    commandHandle <- runCommand $ "nvim " ++ unwords filenames
    _ <- waitForProcess commandHandle
    return ()

-- run the project
ideRun :: [String] -> IO()
{-ideRun :: IO ()-}
ideRun _ = do
    (projectType, _) <- getProjectAttributes
    case projectType of
        Just Haskell -> ideRunCommand "./out" []
        Just Python -> ideRunCommand "python" ["main.py"]
        _ -> putStrLn "Language does not support running"


-- delete a project
ideDeleteProject :: [String] -> IO ()
ideDeleteProject _ = do
    projExists <- projectExists
    when projExists ( do
        setRed
        putStrLn "Deleting .project file"
        exists <- doesFileExist ".project"
        when exists $ removeFile ".project"
        colorReset
        )

-- other
commandSubstitute :: String -> String
commandSubstitute s = result
    where
        wList = words s
        result
            | null wList                                   = s
            | map toLower (head wList) `elem` exitCommands = "exit"
            | otherwise                                    = unwords (wordSubstitute (map toLower $ head wList) : tail wList)

wordSubstitute :: String -> String
wordSubstitute "ls" = "list"
wordSubstitute s = s

exitCommands :: [String]
exitCommands = ["exit", ":q", "quit"]

printError :: String -> IO ()
printError err = setRed >> putStrLn err >> colorReset

prompt :: String
prompt = "no project > "

printPrompt :: IO ()
printPrompt = do
    proj <- projectExists
    if proj then do
        (Just projType, _) <- getProjectAttributes
        setSGR [SetColor Foreground Vivid Green] >> putStr (show projType ++ " > ") >>  colorReset >> hFlush stdout
    else
        setSGR [SetColor Foreground Vivid Green] >> putStr prompt >>  colorReset >> hFlush stdout

-- Color codes
setRed :: IO ()
setRed = setSGR [SetColor Foreground Vivid Red]

colorReset :: IO ()
colorReset = setSGR [Reset]