module AppState (
    AppStateM,
    run
) where

import Prelude (
    class Functor, class Apply, class Applicative, class Bind, class Monad,
    bind, pure, ($), (-)
    )
import Data.Foldable (indexl, length)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup.First (First(First))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)

import Capabilities(class MonadRandom)

newtype AppStateM x = AppStateM (Aff x)

derive newtype instance functorAppStateM :: Functor AppStateM
derive newtype instance applyAppStateM :: Apply AppStateM
derive newtype instance applicativeAppStateM :: Applicative AppStateM
derive newtype instance bindAppStateM :: Bind AppStateM
derive newtype instance monadAppStateM :: Monad AppStateM

instance randomAppStateM :: MonadRandom AppStateM where
    choose cs = do
        i <- AppStateM $ liftEffect $ randomInt 0 $ length cs - 1
        case indexl i cs of
            Just c -> pure c
            Nothing -> let First c = foldMap1 First cs in pure c

    range s e = AppStateM $ liftEffect $ randomInt s e

instance effectAppStateM :: MonadEffect AppStateM where
    liftEffect x = AppStateM $ liftEffect x

instance affAppStateM :: MonadAff AppStateM where
    liftAff = AppStateM

run :: forall x. AppStateM x -> Aff x
run (AppStateM x) = x