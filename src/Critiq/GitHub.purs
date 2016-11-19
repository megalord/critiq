module Critiq.GitHub
  ( comments
  , makeComment
  , pullRequest
  , pullRequests
  , PullRequest(..)
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Array (foldl, snoc)
import Data.Either (either)
import Data.Foreign.Class (readJSON, readProp, class IsForeign)
import Data.String (replaceAll, split, Pattern(..), Replacement(..))
import Neovim.Plugin (PLUGIN)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub.Api (request)
import Critiq.Pane (bufShow, class BufShow)


splitLines :: String -> Array String
splitLines = split (Pattern "\n") <<< replaceAll (Pattern "\r") (Replacement "")

data IssueComment = IssueComment
  { body :: String
  , time :: String
  , user :: String
  }

instance commentIsForeign :: IsForeign IssueComment where
  read value = do
    body <- readProp "body" value
    time <- readProp "created_at" value

    userObj <- readProp "user" value
    user <- readProp "login" userObj
    pure $ IssueComment { body: body, time: time, user: user }

instance bufShowIssueComment :: BufShow IssueComment where
  bufShow (IssueComment comment) = [ "From " <> comment.user <> " on " <> comment.time ] <> splitLines comment.body

data PRComment = PRComment
  { body :: String
  , diff :: String
  , path :: String
  , time :: String
  , user :: String
  }

instance prCommentIsForeign :: IsForeign PRComment where
  read value = do
    body <- readProp "body" value
    diff <- readProp "diff_hunk" value
    path <- readProp "path" value
    time <- readProp "created_at" value

    userObj <- readProp "user" value
    user <- readProp "login" userObj
    pure $ PRComment { body: body, diff: diff, path: path, time: time, user: user }

instance bufShowPRComment :: BufShow PRComment where
  bufShow (PRComment comment) = [ "From " <> comment.user <> " on " <> comment.time
                                , comment.path
                                ] <> splitLines comment.diff <> splitLines comment.body

makeComment :: forall e. Int -> String -> Aff (http :: HTTP | e) Unit
makeComment n c = pure unit

data PullRequest = PullRequest
  { body :: String
  , number :: Int
  , state :: String
  , title :: String
  }

instance pullRequestIsForeign :: IsForeign PullRequest where
  read value = do
    body <- readProp "body" value
    number <- readProp "number" value
    state <- readProp "state" value
    title <- readProp "title" value
    pure $ PullRequest { body: body, number: number, state: state, title: title }

instance bufShowPullRequest :: BufShow PullRequest where
  bufShow (PullRequest pr) = [ pr.title <> " #" <> show pr.number ] <> splitLines pr.body

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

--type Diff =
--  { removed :: 
--parseDiffHunk :: String -> Diff
--filename
--filter startsWith ("+" || "-") <<< flatten <<< map (split "\n") <<< filter odd <<< split "@@" $ patch
