module Main where

import Prelude (($), (>>=), (=<<), Unit, bind, discard, pure, unit, class Eq)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(Proxy))

import AppState (run)
import Capabilities (class MonadRandom)
import Rabbit as Rabbit

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI (H.hoist run component) unit

type Slots = (
    one :: Rabbit.Slot Unit,
    two :: Rabbit.Slot Unit,
    three :: Rabbit.Slot Unit
    )

_one :: Proxy "one"
_one = Proxy

_two :: Proxy "two"
_two = Proxy

_three :: Proxy "three"
_three = Proxy

data Action = Initialise | Tick

derive instance eqAction :: Eq Action

timer :: forall m x. MonadAff m => x -> m (HS.Emitter x)
timer x = do
    {emitter, listener} <- H.liftEffect HS.create
    _ <- H.liftAff $ Aff.forkAff $ forever do
        Aff.delay $ Aff.Milliseconds 150.0
        H.liftEffect $ HS.notify listener x
    pure emitter

component :: forall q m. MonadRandom m => MonadAff m => H.Component q Unit Unit m
component = H.mkComponent {
    initialState : pure unit,
    render : pure $ HH.div
        [
            HP.id "field"
        ]
        [
            HH.slot_ _one unit (Rabbit.component "./Images/Rab") Nothing,
            HH.slot_ _two unit (Rabbit.component "./Images/2Rab") Nothing,
            HH.slot_ _three unit (Rabbit.component "./Images/3Rab") Nothing
        ]
    , eval : H.mkEval $ H.defaultEval {
        handleAction = \a -> case a of
            Initialise -> do
                _ <- H.subscribe =<< timer Tick
                pure unit
            Tick -> do
                _ <- H.query _one unit $ Rabbit.Tick unit
                _ <- H.query _two unit $ Rabbit.Tick unit
                _ <- H.query _three unit $ Rabbit.Tick unit
                pure unit
        ,
        initialize = Just Initialise
        }
    }