module Critiq.GitHub
  ( comments
  , makeComment
  , pullRequest
  , pullRequests
  , PullRequest(..)
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Either (either, Either(Left, Right))
import Data.Foreign.Class (readJSON, readProp, class IsForeign)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)

import Critiq.GitHub.Api (request)


type Comment =
  { content :: String
  , line :: Int
  , time :: String
  , user :: String
  }

comments :: forall e. Int -> Aff (http :: HTTP | e) (Array Comment)
comments n = pure []

makeComment :: forall e. Int -> String -> Aff (http :: HTTP | e) Unit
makeComment n c = pure unit

data PullRequest = PullRequest
  { body :: String
  , state :: String
  }

instance pullRequestIsForeign :: IsForeign PullRequest where
  read value = do
    body <- readProp "body" value
    state <- readProp "state" value
    pure $ PullRequest { body: body, state: state }

pullRequest :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS | e) (Either String PullRequest)
pullRequest n = readJSON' <$> (request "GET" ("/pulls/" <> show n))
  where readJSON' = either (Left <<< show) Right <<< runExcept <<< readJSON

pullRequests :: forall e. Aff (http :: HTTP | e) (Array PullRequest)
pullRequests = pure []

--type Diff =
--  { removed :: 
--parseDiffHunk :: String -> Diff
--filename
--filter startsWith ("+" || "-") <<< flatten <<< map (split "\n") <<< filter odd <<< split "@@" $ patch
