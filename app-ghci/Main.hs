module Main where

import Common (escapeLatexChars)
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
  . (++ "\n\\end{repl}\n")
  . intercalate "\n\n"
  . zipping isSilent
            (\a -> mconcat [ "\\ghcisilent{"
                           , escapeLatexChars a
                           , "}"
                           ])
            (\a b -> mconcat [ "\\ghci{"
                             , escapeLatexChars a
                             , "}{"
                             , escapeLatexChars $ initNonEmpty b
                             , "}"
                             ])
            (fmap (dropWhile isSpace) as)


zipping :: (a -> Bool) -> (a -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipping _ _ _ [] _ = []
zipping _ _ _ _ [] = []
zipping p d f (a:as) bs | p a = d a   : zipping p d f as bs
zipping p d f (a:as) (b:bs)   = f a b : zipping p d f as bs


initNonEmpty :: [a] -> [a]
initNonEmpty [] = []
initNonEmpty a = init a


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


isSilent :: String -> Bool
isSilent str
  | isPrefixOf ":set " str = True
  | isPrefixOf "let " str = True
  | otherwise = False



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
  [ ":set -XDataKinds"
  , ":kind! Functor"
  , ":kind! Monad"
  ]
