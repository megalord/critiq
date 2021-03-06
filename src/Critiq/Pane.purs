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

import Neovim.Buffer (getName, setLines)
import Neovim.Plugin (PLUGIN)
import Neovim.Types (Buffer, Nvim)
import Neovim.Nvim (command, eval, listBufs, getCurrentBuf)

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
writeLines b lines = setLines b 0 (length lines) false lines

clear :: forall e. Nvim -> Aff (plugin :: PLUGIN | e) Unit
clear = const (pure unit) <=< flip command "normal ggdG"

openNew :: forall e. Nvim -> Aff (plugin :: PLUGIN | e) Unit
openNew vim = sequence_ $ map (command vim) setupCmds
  where setupCmds = commands
                 <> map ("setlocal " <> _) settings
                 <> map (\(Tuple key value) -> "nnoremap <buffer> <silent> " <> key <> " :call " <> value) keyMap
                 <> Syntax.commands

openExisting :: forall e. Nvim -> String -> Aff (plugin :: PLUGIN | e) Unit
openExisting vim name = sequence_ $ map (command vim) ["set switchbuf=useopen", "sbuffer " <> name]

openWrite :: forall e. Nvim -> (Array String) -> Aff (plugin :: PLUGIN | e) Unit
openWrite vim lines = open vim >>= (\b -> clear vim >>= \_ -> writeLines b lines)

open vim = (\_ -> getCurrentBuf vim) <=< (maybe' (\_ -> openNew vim) (openExisting vim) <<< existing) <=< (sequence <<< map getName) <=< listBufs $ vim
  where existing = head <<< filter (either (\_ -> const false) test re)
        re = regex (escape bufName <> "$") noFlags
        escape = replaceAll (Pattern "*") (Replacement "\\*")
