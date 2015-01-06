module Print where

import System.Console.ANSI (Color(Green, Red), ColorIntensity(Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)
import System.IO (hFlush, stdout)
import System.FilePath ()

import Functions

-- print an error message
printError :: String -> IO ()
printError err = setRed >> putStrLn err >> colorReset

-- print the prompt
printPrompt :: IDEState -> IO ()
printPrompt (maybeProjectType, _) = do
    setSGR [SetColor Foreground Vivid Green]
    -- (mProjType, _) <- getProjectAttributes
    case maybeProjectType of
        Nothing       -> putStr "no project > "
        Just projType -> putStr (show projType ++ " > ")
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
