{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Effect.Trans.Maybe where

import Prelude hiding (Monad(..))

import Control.Effect
import Control.Effect.Cond
import Control.Effect.Maybe
import Control.Effect.Trans
import Control.Effect.Trans.TF

{- !!!!!!!!!!!!!!!!!!!!!

Make sure to make the type an actual transformer!
The type 'm' doesn't occur anywhere right now!

-}

data IMaybeT :: (k -> * -> *) -> (*,k) -> * -> * where
    IMaybeT :: { unIMT :: m p (IMaybe r a) } -> IMaybeT m '(r,p) a
{-
    INothing ::                  IMaybeT m '(F,p) a 
    IJust    :: m p a         -> IMaybeT m '(T,p) a 
    IDyn     :: IMaybeT m s a -> IMaybeT m '(U,Snd s) a -- dynamic partiality
-}
instance Effect m => Effect (IMaybeT m) where
  type Inv  (IMaybeT m) s t = Inv m (Snd s) (Snd t)
  type Unit (IMaybeT m) = '(T,Unit m)

  type Plus (IMaybeT m) '(F,p) r = '(F,p)
  type Plus (IMaybeT m) '(T,p) r      = r
  type Plus (IMaybeT m) '(U,p) r = '(U,Snd r)

  return x = IMaybeT (return (IJust x))

  IMaybeT m >>= k = IMaybeT $ do
    n <- m
    case n of
      IJust a -> unIMT (k a)
      INothing -> return INothing
      IDyn (IJust a)  -> fmap IDyn (unIMT (k a))
      IDyn (INothing) -> return (IDyn INothing)

{-
instance Trans IMaybeT where
  lift = IJust

instance Cond m => Cond (IMaybeT m) where
    type AltInv (IMaybeT m) s t = ()

    type Alt (IMaybeT m) '(T,s) '(T,r) = '(T,Alt m s r)
    type Alt (IMaybeT m) '(F,s) '(F,r) = '(F,Alt m s r)

    type Alt (IMaybeT m) '(F,s) '(T,r) = '(U,Alt m s r)
    type Alt (IMaybeT m) '(T,s) '(F,r) = '(U,Alt m s r)

    -- statically decidable
    ifM True  (IJust x) (IJust y) = IJust x
    ifM False (IJust x) (IJust y) = IJust y
    ifM True  INothing  INothing  = INothing
    ifM False INothing  INothing  = INothing

    -- dynamic (statically undecidable)
    ifM True  INothing  (IJust x) = IDyn INothing
    ifM False INothing  (IJust x) = IDyn (IJust x)
    ifM True  (IJust x) INothing  = IDyn (IJust x)
    ifM False (IJust x) INothing  = IDyn INothing
-}
