module Common where

import Data.List.Utils (replace)

escapeLatexChars :: String -> String
escapeLatexChars =
  replace "~" "!\\tyeq!"


escapeMoreLatexChars :: String -> String
escapeMoreLatexChars
  = escapeLatexChars
  . replace "{" "\\{"
  . replace "}" "\\}"

