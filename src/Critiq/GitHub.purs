module Critiq.GitHub
  ( comments
  , makeComment
  , pullRequest
  , pullRequests
  , PullRequest(..)
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (either, Either(Left, Right))
import Data.Foreign.Class (readJSON, readProp, class IsForeign)
import Data.Options ((:=), Options)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Neovim.Plugin (debug, PLUGIN)
import Network.HTTP (status2Number)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.HTTP.Client (headers, hostname, method, path, protocol, RequestHeaders(..), RequestOptions)
import Node.OS (OS)
import Node.SimpleRequest as SR

import Critiq.GitHub.Repo (repo)
import Critiq.GitHub.Token (token)


withHeaders :: forall e. String -> Options RequestOptions -> Aff (fs :: FS, os :: OS | e) (Options RequestOptions)
withHeaders domain opts = (\t -> opts <> headers := RequestHeaders (fromFoldable (hs t))) <$> token domain
  where hs t = [ Tuple "Authorization" ("token " <> t)
               , Tuple "User-Agent" "Critiq"
               ]

baseOpts :: Options RequestOptions
baseOpts = method := "GET"
        <> protocol := "https:"

request :: forall e. Options RequestOptions -> Aff (http :: HTTP | e) (Either (SR.Response String) (SR.Response String))
request opts = do
  res <- SR.request opts
  let code = status2Number res.statusCode
  pure ((if code >= 200 && code < 300 then Right else Left) res)

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

reqOpts :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS | e) (Tuple String String)
reqOpts n = (\r -> Tuple ("api." <> r.domain) ("/repos/" <> r.org <> "/" <> r.name <> "/pulls/" <> show n)) <$> repo

pullRequest :: forall e. Int -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS | e) (Either String PullRequest)
pullRequest n = either (\res -> Left res.body) (\res -> readJSON' res.body) <$> (reqOpts n >>= toOpts >>= \o -> debug o >>= \_ -> request o)
  where readJSON' = either (Left <<< show) Right <<< readJSON
        toOpts (Tuple domain p) = withHeaders domain (baseOpts <> hostname := domain <> path := p)

pullRequests :: forall e. Aff (http :: HTTP | e) (Array PullRequest)
pullRequests = pure []

--type Diff =
--  { removed :: 
--parseDiffHunk :: String -> Diff
--filename
--filter startsWith ("+" || "-") <<< flatten <<< map (split "\n") <<< filter odd <<< split "@@" $ patch
