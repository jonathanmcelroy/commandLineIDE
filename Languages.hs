module Languages where

import System.FilePath

data Language = Haskell | Latex | Python
    deriving (Show, Eq, Read)

getLanguage :: String -> Maybe Language
getLanguage "Haskell" = Just Haskell
getLanguage "haskell" = Just Haskell
getLanguage "Latex" = Just Latex
getLanguage "latex" = Just Latex
getLanguage "Python" = Just Python
getLanguage "python" = Just Python
getLanguage _ = Nothing

getFilesLanguage :: [FilePath] -> Maybe Language
getFilesLanguage [] = Nothing
getFilesLanguage [file] = getFileLanguage file
getFilesLanguage (file : others) =
    let other = getFilesLanguage others
    in if getFileLanguage file == other then getFileLanguage file else Nothing

getFileLanguage :: FilePath -> Maybe Language
getFileLanguage file = case takeExtension file of
    ".hs" -> Just Haskell
    ".py" -> Just Python
    ".tex" -> Just Latex
    _ -> Nothing
