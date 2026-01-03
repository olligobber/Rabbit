module Main where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
    , eval : H.mkEval $ H.defaultEval
    }