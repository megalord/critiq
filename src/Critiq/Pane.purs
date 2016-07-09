module Critiq.Pane
  ( open
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Foldable (sequence_)

import Neovim.Plugin (PLUGIN)
import Neovim.Types (Vim)
import Neovim.Vim (command)

commands = [ "botright vertical 60 new"
           , "setlocal noswapfile"
           , "setlocal buftype=nofile"
           , "setlocal bufhidden=hide"
           , "setlocal nobuflisted"
           , "setlocal nospell"
           , "setlocal nonu"
           ]

open :: forall e. Vim -> Aff (plugin :: PLUGIN | e) Unit
open vim = sequence_ (map (command vim) commands)
