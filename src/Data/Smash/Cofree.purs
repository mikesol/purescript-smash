module Data.Smash.Cofree
  ( lift
  , liftWith
  ) where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, tail)
import Data.Functor.Pairing.Co (Co, runCo)
import Data.Smash as S
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Prim.RowList (class RowToList)

liftWith
  :: forall l f r rl rest a
   . IsSymbol l
  => Functor f
  => Row.Cons l (Proxy (Cofree f)) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => Proxy l
  -> Co f a
  -> Co (S.Smash r) a
liftWith l fa = S.cosmash l (runCo fa <<< map extract <<< tail)

lift
  :: forall f r rl a
   . RowToList r rl
  => Functor f
  => S.ComonadSmash rl r
  => Co f a
  -> Co (S.Smash (cofree :: Proxy (Cofree f) | r)) a
lift = liftWith (Proxy :: Proxy "cofree")
