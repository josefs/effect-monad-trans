{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Effect.Trans.CounterNat where

import Control.Effect
import Control.Effect.Trans
import GHC.TypeLits
import Prelude hiding (Monad(..))
import Control.Effect.Trans.TF

data CounterT :: (k -> * -> *) -> (Nat,k) -> * -> * where
  CounterT :: { forget :: m (Snd p) a } -> CounterT m p a

instance Effect m => Effect (CounterT m) where
    type Inv (CounterT m) s p = Inv m (Snd s) (Snd p)
    type Unit (CounterT m) = '(0,Unit m)
    type Plus (CounterT m) s p = '(Fst s + Fst p,Plus m (Snd s) (Snd p))

    return a = CounterT (return a)
    (CounterT m) >>= k = CounterT (m >>= (forget . k))

instance Trans CounterT where
  lift m = CounterT m

tick :: Effect m => a -> CounterT m '(1,Unit m) a
tick x = CounterT (return x)
