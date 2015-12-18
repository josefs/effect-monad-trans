{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Control.Effect.Trans.Counter where

import Control.Effect
import Control.Effect.Trans
import GHC.TypeLits
import Prelude hiding (Monad(..))

data CounterT m '(n :: Nat,p) a = CounterT { forget :: m p a }

instance Effect m => Effect (CounterT m) where
    type Inv (CounterT m) (i,s) (j,p) = Inv m s p
    type Unit (CounterT m) = (0,Unit m)
    type Plus (CounterT m) (i,s) (j,p) = (n + m,Plus m s p)

    return a = CounterT (return a)
    (CounterT m) >>= k = CounterT . m >>= forget . k

tick :: Effect m => a -> CounterT m (1,Unit m) a
tick x = CounterT (return x)
