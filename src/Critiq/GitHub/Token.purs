module Critiq.GitHub.Token
  ( token
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.String (replace, Pattern(..), Replacement(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.OS (homedir, OS)


token :: forall e. String -> Aff (fs :: FS, os :: OS | e) String
token domain = (liftEff homedir) >>= map replaceNewlines <<< readTextFile UTF8 <<< (_  <>  "/.config/nvim/.critiq-" <> domain)
  where replaceNewlines = replace (Pattern "\n") (Replacement "")
