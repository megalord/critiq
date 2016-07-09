module Critiq.GitHub.Repo
  ( repo
  , Repo
  ) where

import Prelude
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Array (filter, head, index)
import Data.Either (either, Either(Left, Right))
import Data.Maybe (maybe, maybe', Maybe(Just, Nothing))
import Data.String (split, take)
import Data.String.Regex (match, noFlags, regex, Regex)
import Node.Buffer (toString, BUFFER)
import Node.ChildProcess (defaultExecOptions, exec, CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))

type Repo =
  { domain :: String
  , org :: String
  , name :: String
  }

execAff :: forall e. String -> Aff (buffer :: BUFFER, cp :: CHILD_PROCESS | e) String
execAff a = makeAff (\error success -> exec a defaultExecOptions (handle error success))
  where handle error success result = maybe' (\_ -> toString UTF8 result.stdout >>= success) error result.error

note :: forall a. String -> Maybe a -> Either String a
note e = maybe (Left e) Right

match' :: Either String Regex -> String -> Maybe (Array (Maybe String))
match' (Left _) = \_ -> Nothing
match' (Right re) = match re

findRemote :: String -> String -> Either String String
findRemote remote = note remoteError <<< head <<< filter (\s -> take 6 s == remote) <<< split "\n"
  where remoteError = "Remote " <> remote <> " not found"

parseRemote :: String -> Either String Repo
parseRemote = note "Remote parsing failed" <<< (_ >>= toRepo) <<< match' (regex re noFlags)
  where re = "@([^:\\/]+)[:\\/]([^\\/]*)\\/([^\\.]*)"
        toRepo matches = do
          mdomain <- index matches 1
          domain <- mdomain
          morg <- index matches 2
          org <- morg
          mname <- index matches 3
          name <- mname
          Just { domain: domain, org: org, name: name }

repo :: forall e. Aff (buffer :: BUFFER, cp :: CHILD_PROCESS | e) Repo
repo =  execAff "git remote -v" >>= (either (throwError <<< error) pure <<< (_ >>= parseRemote) <<< findRemote "origin")
