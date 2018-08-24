module Common where

import Data.List.Utils (replace)

escapeLatexChars :: String -> String
escapeLatexChars =
  replace "~" "!\\tyeq!"
  . replace "{" "\\{"
  . replace "}" "\\}"

