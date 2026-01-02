module Capabilities(
    class MonadRandom, choose, range
) where

import Prelude (class Monad, ($))
import Data.Semigroup.Foldable (class Foldable1)
import Halogen (HalogenM, lift)

class Monad m <= MonadRandom m where
    -- Randomly choose from a list of options with uniform probability
    choose :: forall f x. Foldable1 f => f x -> m x

    -- Randomly choose an integer in an inclusive range with uniform probability
    range :: Int -> Int -> m Int

instance randomHalogenM :: MonadRandom m => MonadRandom (HalogenM s a t o m) where
    choose cs = lift $ choose cs
    range s e = lift $ range s e
