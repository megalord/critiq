module Critiq.GitHub
  ( changedFiles
  , comments
  , makeComment
  , pullRequest
  , pullRequests
  , module Exports
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Array (foldl, snoc)
import Data.Maybe (maybe', Maybe(..))
import Data.Either (either)
import Data.Foreign.Class (readJSON, class IsForeign)
import Data.StrMap (alter, empty, StrMap)
import Neovim.Plugin (PLUGIN)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub.Api (request)
import Critiq.Github.Types (ChangedFile, IssueComment, PullRequest, PRComment(..))
import Critiq.Github.Types (PullRequest(..)) as Exports
import Critiq.Pane (bufShow)


groupBy :: forall a. (a -> String) -> (Array a) -> StrMap (Array a)
groupBy keyFn = foldl (\d x -> alter (append x) (keyFn x) d) empty
  where append x = maybe' (\_ -> Just [x]) (Just <<< flip snoc x)

commentGroups :: Array PRComment -> StrMap (Array PRComment)
commentGroups cs = groupBy (\(PRComment c) -> c.path <> ":" <> show c.position) cs

makeComment :: forall e. Int -> String -> Aff (http :: HTTP | e) Unit
makeComment n c = pure unit

readJSON' :: forall a e. (IsForeign a) => String -> Aff e a
readJSON' = either (throwError <<< error <<< show) pure <<< runExcept <<< readJSON

pullRequest :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                                     plugin :: PLUGIN | e) PullRequest
pullRequest n = readJSON' =<< request "GET" ("/pulls/" <> show n)

pullRequests :: forall e. Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                               plugin :: PLUGIN | e) (Array PullRequest)
pullRequests = readJSON' =<< request "GET" "/pulls"

pullRequestComments :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                                             plugin :: PLUGIN | e) (Array PRComment)
pullRequestComments n = readJSON' =<< request "GET" ("/pulls/" <> show n <> "/comments")

issueComments :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                                       plugin :: PLUGIN | e) (Array IssueComment)
issueComments n = readJSON' =<< request "GET" ("/issues/" <> show n <> "/comments")

comments :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                                  plugin :: PLUGIN | e) (Array String)
comments n = append <$> xs n <*> ys n
  where xs n = foldl (\b a -> b <> snoc (bufShow a) "") [] <$> issueComments n
        ys n = foldl (\b a -> b <> snoc (bufShow a) "") [] <$> pullRequestComments n

changedFiles :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS,
                                      plugin :: PLUGIN | e) (Array ChangedFile)
changedFiles n = readJSON' =<< request "GET" ("/pulls/" <> show n <> "/files")

--type Diff =
--  { removed :: 
--parseDiffHunk :: String -> Diff
--filename
--filter startsWith ("+" || "-") <<< flatten <<< map (split "\n") <<< filter odd <<< split "@@" $ patch
