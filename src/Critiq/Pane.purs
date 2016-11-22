module Critiq.Pane
  ( bufShow
  , open
  , openWrite
  , class BufShow
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Array (filter, head, length)
import Data.Either (either)
import Data.Foldable (sequence_)
import Data.Maybe (maybe')
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

import Neovim.Buffer (getName, setLineSlice)
import Neovim.Plugin (PLUGIN)
import Neovim.Types (Buffer, Vim)
import Neovim.Vim (command, eval, getBuffers, getCurrentBuffer)

import Critiq.Syntax as Syntax


class BufShow a where
  bufShow :: a -> Array String

bufName = "*Critiq*"

commands = [ "botright vertical 80 new " <> bufName
           ]

settings = [ "noswapfile"
           , "buftype=nofile"
           , "bufhidden=wipe"
           , "filetype=markdown"
           , "nobuflisted"
           , "nospell"
           , "nonumber"
           --, "nomodifiable" must do after finished writing
           ]

keyMap = [ Tuple "<enter>" "CritiqSelectPR()" ]

writeLines :: forall e. Buffer -> (Array String) -> Aff (plugin :: PLUGIN | e) Unit
writeLines b lines = setLineSlice b 0 (length lines) true false lines

clear :: forall e. Vim -> Aff (plugin :: PLUGIN | e) Unit
clear = const (pure unit) <=< flip command "normal ggdG"

openNew :: forall e. Vim -> Aff (plugin :: PLUGIN | e) Unit
openNew vim = sequence_ $ map (command vim) setupCmds
  where setupCmds = commands
                 <> map ("setlocal " <> _) settings
                 <> map (\(Tuple key value) -> "nnoremap <buffer> <silent> " <> key <> " :call " <> value) keyMap
                 <> Syntax.commands

openExisting :: forall e. Vim -> String -> Aff (plugin :: PLUGIN | e) Unit
openExisting vim name = sequence_ $ map (command vim) ["set switchbuf=useopen", "sbuffer " <> name]

openWrite :: forall e. Vim -> (Array String) -> Aff (plugin :: PLUGIN | e) Unit
openWrite vim lines = open vim >>= (\b -> clear vim >>= \_ -> writeLines b lines)

open vim = (\_ -> getCurrentBuffer vim) <=< (maybe' (\_ -> openNew vim) (openExisting vim) <<< existing) <=< (sequence <<< map getName) <=< getBuffers $ vim
  where existing = head <<< filter (either (\_ -> const false) test re)
        re = regex (escape bufName <> "$") noFlags
        escape = replaceAll (Pattern "*") (Replacement "\\*")
