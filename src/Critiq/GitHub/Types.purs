module Critiq.Github.Types
  ( ChangedFile(..)
  , IssueComment(..)
  , PRComment(..)
  , PullRequest(..)
  ) where

import Prelude
import Data.Array (replicate)
import Data.Foreign.Class (readProp, class IsForeign)
import Data.String (joinWith, replaceAll, split, Pattern(..), Replacement(..))

import Critiq.Pane (class BufShow)


splitLines :: String -> Array String
splitLines = split (Pattern "\n") <<< replaceAll (Pattern "\r") (Replacement "")

renderBody :: String -> Array String
renderBody = map ("\" " <> _) <<< splitLines

indent :: Int -> String
indent = joinWith "" <<< flip replicate "\t"


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
  bufShow (IssueComment comment) = [ "From " <> comment.user <> " on " <> comment.time ] <> renderBody comment.body


data PRComment = PRComment
  { body :: String
  , diff :: String
  , path :: String
  , position :: Int
  , time :: String
  , user :: String
  }

instance prCommentIsForeign :: IsForeign PRComment where
  read value = do
    body <- readProp "body" value
    diff <- readProp "diff_hunk" value
    path <- readProp "path" value
    position <- readProp "position" value
    time <- readProp "created_at" value

    userObj <- readProp "user" value
    user <- readProp "login" userObj
    pure $ PRComment { body: body, diff: diff, path: path, position: position, time: time, user: user }

instance bufShowPRComment :: BufShow PRComment where
  bufShow (PRComment comment) = [ "From " <> comment.user <> " on " <> comment.time
                                , comment.path
                                ] <> splitLines comment.diff <> renderBody comment.body


data PRCommentGroup = PRCommentGroup
  { comments :: Array PRComment
  , diff :: String
  , path :: String
  , position :: Int
  }


data ChangedFile = ChangedFile
  { additions :: Int
  , name :: String
  , deletions :: Int
  }

instance changedFileIsForeign :: IsForeign ChangedFile where
  read value = do
    additions <- readProp "additions" value
    name <- readProp "filename" value
    deletions <- readProp "deletions" value
    pure $ ChangedFile { additions: additions, name: name, deletions: deletions }

instance bufShowChangedFile :: BufShow ChangedFile where
  bufShow (ChangedFile f) = [ "* " <> f.name <> indent 10 <> "+" <> show f.additions <> "/-" <> show f.deletions ]


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
  bufShow (PullRequest pr) = [ "# " <> pr.title <> " #" <> show pr.number ] <> renderBody pr.body
