module Critiq.Pane
  ( open
  , openWrite
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Array (length)
import Data.Foldable (sequence_)
import Data.Tuple (Tuple(..))

import Neovim.Buffer (setLineSlice)
import Neovim.Plugin (PLUGIN)
import Neovim.Types (Vim)
import Neovim.Vim (command, getCurrentBuffer)

commands :: Array String
commands = [ "botright vertical 60 new"
           ]

--https://github.com/scrooloose/nerdtree/blob/master/lib/nerdtree/creator.vim#L282
--https://github.com/scrooloose/nerdtree/blob/master/plugin/NERD_tree.vim#L160
settings :: Array String
settings = [ "noswapfile"
           , "buftype=nofile"
           , "bufhidden=delete"
           , "nobuflisted"
           , "nospell"
           , "nonu"
           ]

keyMap :: Array (Tuple String String)
keyMap = [ Tuple "<enter>" "CritiqSelectPR()" ]

-- exec 'nnoremap <buffer> <silent> '. self.key . premap . ':call nerdtree#ui_glue#invokeKeyMap("'. keymapInvokeString .'")<cr>'

writeLines :: forall e. Vim -> (Array String) -> Aff (plugin :: PLUGIN | e) Unit
writeLines vim lines = getCurrentBuffer vim >>= \b -> setLineSlice b 0 (length lines) true false lines

open :: forall e. Vim -> Aff (plugin :: PLUGIN | e) Unit
open vim = sequence_ $ map (command vim) setupCmds
  where setupCmds = commands
                 <> map ("setlocal " <> _) settings
                 <> map (\(Tuple key value) -> "nnoremap <buffer> <silent> " <> key <> " :call " <> value) keyMap

openWrite :: forall e. Vim -> (Array String) -> Aff (plugin :: PLUGIN | e) Unit
openWrite vim lines = open vim >>= \_ ->  writeLines vim lines
