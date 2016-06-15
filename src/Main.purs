module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Neovim.Plugin (commandSetLine, PLUGIN)

main :: forall e. Eff (plugin :: PLUGIN | e) Unit
main = commandSetLine "Foo" "foo"
