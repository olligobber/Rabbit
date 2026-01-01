module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA

import Rabbit as Rabbit

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI component

type Slots = (
    one :: Rabbit.Slot Unit,
    two :: Rabbit.Slot Unit,
    three :: Rabbit.Slot Unit
    )

_one :: SProxy "one"
_one = SProxy

_two :: SProxy "two"
_two = SProxy

_three :: SProxy "three"
_three = SProxy

component :: forall q m. H.Component q Unit Unit m
component = H.mkComponent {
    initialState : unit,
    render : pure $ HH.div
        [

        ]
        [
            HH.slot _one unit (Rabbit.component "Images/Rab") unit,
            HH.slot _two unit (Rabbit.component "Images/Rab2") unit,
            HH.slot _three unit (Rabbit.component "Images/Rab3") unit
        ]
    , eval : pure $ pure unit
    }