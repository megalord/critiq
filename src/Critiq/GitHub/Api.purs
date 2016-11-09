module Critiq.GitHub.Api
  ( request
  ) where

import Prelude
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Options ((:=), Options)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Node.HTTP.Client as Client
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.OS (OS)
import Node.Stream (end, Readable)

import Critiq.GitHub.Repo (repo)
import Critiq.GitHub.Token (token)


foreign import collapseStream :: forall w e. Readable w e
                              -> (Error -> Eff e Unit)
                              -> (String -> Eff e Unit)
                              -> Eff e Unit

collapseStreamAff :: forall w e. Readable w e -> Aff e String
collapseStreamAff = makeAff <<< collapseStream

withHeaders :: forall e. String -> Options Client.RequestOptions -> Aff (fs :: FS, os :: OS | e) (Options Client.RequestOptions)
withHeaders domain opts = (\t -> opts <> Client.headers := Client.RequestHeaders (fromFoldable (hs t))) <$> token domain
  where hs t = [ Tuple "Authorization" ("token " <> t)
               , Tuple "User-Agent" "Critiq"
               ]

reqOpts :: forall e. String -> String -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, os :: OS | e) (Options Client.RequestOptions)
reqOpts method path = repo >>= \r -> withHeaders ("api." <> r.domain) (
     Client.protocol := "https:"
  <> Client.hostname := ("api." <> r.domain)
  <> Client.method := method
  <> Client.path := ("/repos/" <> r.org <> "/" <> r.name <> path))

requestAff :: forall e. Options Client.RequestOptions -> Aff (http :: HTTP | e) Client.Response
requestAff opts = makeAff (\error success -> Client.request opts success >>= endReq)
  where endReq req = end (Client.requestAsStream req) (pure unit)

request :: forall e. String -> String -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS, fs :: FS, http :: HTTP, os :: OS | e) String
request method path = reqOpts method path >>= requestAff >>= collapseStreamAff <<< Client.responseAsStream
