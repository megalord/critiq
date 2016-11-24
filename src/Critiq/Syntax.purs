module Critiq.Syntax
  ( commands
  ) where

import Prelude
import Neovim.Plugin (PLUGIN)
import Neovim.Types (Nvim)


syntax = [ "CritiqComment #^\" .*#"
         , "CritiqDiffMinus #^-.*$#"
         , "CritiqDiffPlus #^+.*$#"
         , "CritiqPatch #^@@.*$#"
         ]

highlights = [ "link CritiqComment String"
             , "CritiqDiffMinus ctermbg=52 guibg=#5f0000"
             , "CritiqDiffPlus ctermbg=22 guibg=#005f00"
             , "CritiqPatch ctermfg=244"
             ]

commands :: Array String
commands = (map ("syn match " <> _) syntax) <> (map ("hi " <> _) highlights)
