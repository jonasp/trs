module Expression.Bidirectional where

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.), pred, flip)

data Iso m a b = Iso
  { runForward :: Kleisli m a b
  , runBackward :: Kleisli m b a
  }

instance (Monad m) => Category (Iso m) where
  id = Iso (Kleisli pure) (Kleisli pure)
  Iso a1 b1 . Iso a2 b2 = Iso (a1 . a2) (b2 . b1)

class IsoFunctor f where
  (<$>) :: Iso m a b -> (f a -> f b)
