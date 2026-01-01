module Capabilities(
	MonadRandom(choose),
) where

class MonadRandom m <= Monad m where
	-- Randomly choose from a list of options with uniform probability
	choose :: forall f x. Foldable f => f x -> m x

	-- Randomly choose an integer in an inclusive range with uniform probability
	range :: Int -> Int -> m Int
	range s e = choose $ s..e

