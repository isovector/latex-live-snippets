module Main where

import Data.Char (isSpace)
import Control.Lens ((^.), _head, (%~))
import Control.Monad (guard, join)
import Data.List
import Data.List.Utils (replace)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import System.Directory
import System.Environment
import System.FilePath.Lens (basename)
import System.FilePath.Posix


import Debug.Trace

main :: IO ()
main = do
  let dir = ".latex-live-snippets/repl"

  [samplefilename, responsefilename, uniqueid] <- getArgs
  sample <- readFile samplefilename
  response <- readFile responsefilename
  createDirectoryIfMissing True dir
  let filename' = dir </> uniqueid ++ ".tex"
  writeFile filename' $ interleave (lines sample) $ responses response


interleave :: [String] -> [String] -> String
interleave as
  = ("\\begin{repl}\n" ++)
  . (++ "\n\\end{repl}")
  . intercalate "\n\n"
  . zipWith (\a b -> mconcat ["\\ghci{", a, "}{", init b, "}"])
            (fmap (dropWhile isSpace) as)


responses :: String -> [String]
responses
  = fmap unlines
  . fmap (_head %~ removeTag)
  . groupBy (\_ a -> not $ isResponse a)
  . tail
  . dropWhile (not . isPrefixOf "Ok, modules loaded")
  . lines


removeTag :: String -> String
removeTag = drop 2 . dropWhile (/= '>')


isResponse :: String -> Bool
isResponse ('*':_) = True
isResponse _ = False



test :: String
test = unlines
  [ "GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help"
  , "Prelude> [1 of 1] Compiling RankN            ( code/RankN.hs, interpreted )"
  , "Ok, modules loaded: RankN."
  , "*RankN> Functor :: (* -> *) -> Constraint"
  , "= Functor"
  , "*RankN> Monad :: (* -> *) -> Constraint"
  , "= Monad"
  , "*RankN> Leaving GHCi."
  ]


sample :: String
sample = unlines
  [ ":load code/RankN.hs"
  , ":kind! Functor"
  , ":kind! Monad"
  ]
