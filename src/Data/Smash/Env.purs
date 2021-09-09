module Data.Smash.Env
  ( ask
  , askWith
  ) where

import Control.Comonad (extract)
import Control.Comonad.Env.Class as Env
import Data.Functor.Pairing.Co (Co)
import Data.Smash as S
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Prim.RowList (class RowToList)

askWith
  :: forall l w r rl rest a
   . Env.ComonadEnv a w
  => IsSymbol l
  => Row.Cons l (Proxy w) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => Proxy l
  -> Co (S.Smash r) a
askWith l = S.cosmash l \wa -> extract wa (Env.ask wa)

ask
  :: forall w r rl a
   . Env.ComonadEnv a w
  => RowToList r rl
  => S.ComonadSmash rl r
  => Co (S.Smash (env :: Proxy w | r)) a
ask = askWith (Proxy :: Proxy "env")
