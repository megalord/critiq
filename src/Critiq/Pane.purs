module Critiq.Pane
  ( open
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Foldable (sequence_)

import Neovim.Plugin (function, PLUGIN)
import Neovim.Types (Vim)
import Neovim.Vim (command, getCurrentLine)

commands :: Array String
commands = [ "botright vertical 60 new"
           ]

--https://github.com/scrooloose/nerdtree/blob/master/lib/nerdtree/creator.vim#L282
settings :: Array String
settings = [ "noswapfile"
           , "buftype=nofile"
           , "bufhidden=delete"
           , "nobuflisted"
           , "nospell"
           , "nonu"
           ]

--keyMap = [ Tuple ("<cr>" "critiq#selectPR") ]
-- map ("nnoremap <buffer> <silent> " <> key <> value) keyMap

-- exec 'nnoremap <buffer> <silent> '. self.key . premap . ':call nerdtree#ui_glue#invokeKeyMap("'. keymapInvokeString .'")<cr>'


open :: forall e. Vim -> Aff (plugin :: PLUGIN | e) Unit
open vim = sequence_ $ map (command vim) (commands <> (map ("setlocal " <> _) settings))

--function "critiq#selectPR"
--  getCurrentLine vim >>= command CritiqPR parse
