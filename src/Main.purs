module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Array (concat, head, uncons)
import Data.Int (fromString)
import Data.Maybe (maybe, maybe', Maybe)
import Data.String (split, Pattern(..))
import Data.Traversable (sequence)
import Neovim.Plugin (command, defaultOpts, function, Args, Range, PLUGIN)
import Neovim.Types (Nvim)
import Neovim.Nvim (getCurrentLine, errWrite)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub
import Critiq.Pane (bufShow, openWrite)


main = sequence (
  [ command "CritiqPR" defaultOpts handle
  , function "CritiqSelectPR" (\vim _ -> getCurrentLine vim >>= maybe (errWrite vim "Not called on Pull Request") (pull vim) <<< prNumberFromLine)
  ])

prNumberFromLine :: String -> Maybe Int
prNumberFromLine = fromString <=< head <<< split (Pattern ")")

handle :: forall e. Nvim -> Args -> Range -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP,
                                                 os :: OS, plugin :: PLUGIN | e) Unit
handle vim args _ = maybe' (\_ -> pulls vim) parseArgs (uncons args)
  where parseArgs { head: num } = maybe' (\_ -> errWrite vim (num <> " is not a number")) (pull vim) (fromString num)

pull :: forall e. Nvim -> Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                                     plugin :: PLUGIN | e) Unit
pull vim x = map concat (sequence [ map bufShow (pullRequest x), (concat <<< map bufShow) <$> (changedFiles x), comments x ]) >>= openWrite vim

pulls :: forall e. Nvim -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                               plugin :: PLUGIN | e) Unit
pulls vim = pullRequests >>= openWrite vim <<< map showTitle
  where showTitle (PullRequest pr) = show pr.number <> ") " <> pr.title

--augroup uncompress
--  au!
--  au BufEnter *.gz	%!gunzip
--augroup END
