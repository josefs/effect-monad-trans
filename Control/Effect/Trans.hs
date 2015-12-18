{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Control.Effect.Trans where

import Control.Effect
import Control.Effect.Trans.TF

class Trans t where
  lift :: (Effect (t m), Effect m) => m p a -> t m '(Fst (Unit (t m)),p) a
