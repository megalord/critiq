module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Array (length, uncons)
import Data.Either (either)
import Data.Int (fromString)
import Data.Maybe (maybe')
import Data.String (split, Pattern(..))
import Neovim.Buffer (setLineSlice)
import Neovim.Plugin (command, defaultOpts, Args, Range, PLUGIN)
import Neovim.Types (Vim)
import Neovim.Vim (getCurrentBuffer, reportError)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub (pullRequest, pullRequests, PullRequest(..))
import Critiq.Pane (open)


main :: forall e. Eff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
main = command "CritiqPR" defaultOpts handle

handle :: forall e. Vim -> Args -> Range -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
handle vim args _ = maybe' (\_ -> pulls vim) parseArgs (uncons args)
  where parseArgs { head: num } = maybe' (\_ -> reportError vim (num <> " is not a number")) (pull vim) (fromString num)

pull :: forall e. Vim -> Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
pull vim x = pullRequest x >>= either (reportError vim) (\(PullRequest pr) -> open vim >>= \_ -> (writeToLines <<< split (Pattern "\n")) pr.body)
  where writeToLines lines = getCurrentBuffer vim >>= \b -> setLineSlice b 0 (length lines) true false lines

pulls :: forall e. Vim -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
pulls vim = pullRequests >>= either (reportError vim) (\prs -> open vim >>= \_ -> writeToLines (map showTitle prs))
  where showTitle (PullRequest pr) = show pr.number <> ") " <> pr.title
        writeToLines lines = getCurrentBuffer vim >>= \b -> setLineSlice b 0 (length lines) true false lines

--augroup uncompress
--  au!
--  au BufEnter *.gz	%!gunzip
--augroup END
