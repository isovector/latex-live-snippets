module Common where

import Data.List.Utils (replace)

escapeLatexChars :: String -> String
escapeLatexChars
  = replace "$" "\\$"
  . replace "{" "\\{"
  . replace "}" "\\}"
  . replace "#" "\\#"
  . replace "~" "\\tyeq"
  . replace "\\" "\\textbackslash\\!\\! "

