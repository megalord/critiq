module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Array (length, uncons)
import Data.Either (either)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (split)
import Neovim.Buffer (setLineSlice)
import Neovim.Plugin (command, defaultOpts, Args, Range, PLUGIN)
import Neovim.Types (Vim)
import Neovim.Vim (getCurrentBuffer, reportError)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub (pullRequest, PullRequest(..))
import Critiq.Pane (open)


main :: forall e. Eff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
main = command "CritiqPR" defaultOpts handle

handle :: forall e. Vim -> Args -> Range -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
handle vim args _ = case uncons args of
                         Just { head: num } -> case fromString num of
                                                    Just x -> pull vim x
                                                    Nothing -> reportError vim (num <> " is not a number")
                         Nothing -> reportError vim "PR number argument required"

pull :: forall e. Vim -> Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
pull vim x = pullRequest x >>= either (reportError vim) (\(PullRequest pr) -> open vim >>= \_ -> (writeToLines <<< split "\r\n") pr.body)
  where writeToLines lines = getCurrentBuffer vim >>= \b -> setLineSlice b 0 (length lines) true false lines

--topleft vertical 60 new
--https://github.com/scrooloose/nerdtree/blob/master/lib/nerdtree/creator.vim#L282

--augroup uncompress
--  au!
--  au BufEnter *.gz	%!gunzip
--augroup END
