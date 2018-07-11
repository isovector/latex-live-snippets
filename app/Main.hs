module Main where

import System.Environment
import System.Directory
import System.FilePath.Posix
import System.FilePath.Lens (basename)
import Control.Lens ((^.))
import Data.List
import Data.List.Utils (replace)


main :: IO ()
main = do
  let dir = ".latex-live-snippets"

  [filename, decl] <- getArgs
  file <- readFile filename
  createDirectoryIfMissing True dir
  let filename' = dir </> filename ^. basename ++ "." ++ decl ++ ".tex"
  writeFile filename' $ getDefinition file decl


matchDefinition :: String -> String -> Bool
matchDefinition decl line =
  or $ do
    form <- [ ""
            , "-- # "
            , "type family "
            , "data family "
            , "data "
            , "type "
            , "newtype "
            ]
    pure $ isPrefixOf (form ++ decl) line


getDefinition :: String -> String -> String
getDefinition file decl
    = unlines
    . ("\\begin{code}" :)
    . (++ ["\\end{code}"])
    . fmap annotate
    . fmap (replace "$" "\\$")
    . fmap (replace "{" "\\{")
    . fmap (replace "}" "\\}")
    . fmap (replace "#" "\\#")
    . fmap (replace "~" "\\tyeq")
    . fmap (replace "\\" "\\textbackslash")
    . takeWhile (not . null)
    $ dropWhile (not . matchDefinition decl) ls
  where
    ls = lines file


annotate :: String -> String
annotate [] = ""
annotate ('-':'-':' ':'!':' ':zs) = "\\ann{" ++ zs ++ "}"
annotate (a:as) = a : annotate as

