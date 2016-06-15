module Neovim.Plugin where

import Prelude (Unit)
import Control.Monad.Eff (Eff())

foreign import data PLUGIN :: !

foreign import plugin :: forall eff. Eff (plugin :: PLUGIN | eff) Unit

foreign import commandSetLine :: forall e. String -> String -> Eff (plugin :: PLUGIN | e) Unit
