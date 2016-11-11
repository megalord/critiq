module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Array (head, uncons)
import Data.Either (either)
import Data.Foldable (sequence_)
import Data.Int (fromString)
import Data.Maybe (maybe, maybe', Maybe)
import Data.String (split, Pattern(..))
import Neovim.Plugin (command, defaultOpts, function, Args, Range, PLUGIN)
import Neovim.Types (Vim)
import Neovim.Vim as Nvim
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub (pullRequest, pullRequests, PullRequest(..))
import Critiq.Pane (openWrite)


main :: forall e. Eff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                       plugin :: PLUGIN | e) Unit
main = sequence_ (
  [ command "CritiqPR" defaultOpts handle
  , function "CritiqSelectPR" (\vim _ -> Nvim.getCurrentLine vim >>= maybe (pure unit) (pull vim) <<< prNumberFromLine)
  ])

prNumberFromLine :: String -> Maybe Int
prNumberFromLine = fromString <=< head <<< split (Pattern ")")

handle :: forall e. Vim -> Args -> Range -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS,
                                                 http :: HTTP, os :: OS, plugin :: PLUGIN | e) Unit
handle vim args _ = maybe' (\_ -> pulls vim) parseArgs (uncons args)
  where parseArgs { head: num } = maybe' (\_ -> Nvim.reportError vim (num <> " is not a number")) (pull vim) (fromString num)

withRight vim f = either (Nvim.reportError vim) (openWrite vim <<< f)

pull :: forall e. Vim -> Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP,
                                     os :: OS, plugin :: PLUGIN | e) Unit
pull vim x = pullRequest x >>= withRight vim (split (Pattern "\n") <<< \(PullRequest pr) -> pr.body)

pulls :: forall e. Vim -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP,
                               os :: OS, plugin :: PLUGIN | e) Unit
pulls vim = pullRequests >>= withRight vim (map showTitle)
  where showTitle (PullRequest pr) = show pr.number <> ") " <> pr.title

--augroup uncompress
--  au!
--  au BufEnter *.gz	%!gunzip
--augroup END
