module Common where

import Data.List.Utils (replace)

escapeLatexChars :: String -> String
escapeLatexChars =
  replace "~" "!\\tyeq!"


escapeGHCILatexChars :: String -> String
escapeGHCILatexChars
  = escapeLatexChars
  . replace "!!!!!!!!!!" "\\"
  . replace "\\" "!!!!!!!!!!textbackslash{}"
  . replace "{" "!!!!!!!!!!{"
  . replace "}" "!!!!!!!!!!}"

