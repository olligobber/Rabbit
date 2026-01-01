module Rabbit (
    Slot,
    component
) where

import Prelude (
    class Eq, class Show,
    mod, negate, pure, bind, show,
    ($), (==), (+), (<>), (<), (>), (<=), (>=), (&&), (*),
    Unit, Void
    )
import Data.Foldable (fold)
import Data.NonEmpty (NonEmpty(NonEmpty))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Capabilities (class MonadRandom, choose, range)

data State = Standing | Sitting | Eating | EarFlop | Walking

derive instance eqState :: Eq State

data Direction = Left | Right

derive instance eqDirection :: Eq Direction

data Texture = BackLeg | Down | FrontLeg | Nom | Up | UpEar

derive instance eqTexture :: Eq Texture

instance Show Texture where
    show BackLeg = "BackLeg"
    show Down = "Down"
    show FrontLeg = "FrontLeg"
    show Nom = "Nom"
    show Up = "Up"
    show UpEar = "UpEar"

data Rabbit = Rabbit {
    state :: State,
    direction :: Direction,
    x :: Int,
    y :: Int,
    timeInState :: Int
    }

texture :: Rabbit -> Texture
texture (Rabbit r) = case r.state of
    Standing -> Up
    Sitting -> Down
    Eating -> case r.timeInState `mod` 2 of
        0 -> Down
        _ -> Nom
    EarFlop -> UpEar
    Walking -> case r.timeInState `mod` 3 of
        0 -> Up
        1 -> FrontLeg
        _ -> BackLeg

idle :: Rabbit -> Rabbit
idle (Rabbit r) = Rabbit $ r { timeInState = r.timeInState + 1 }

gotoState :: State -> Rabbit -> Rabbit
gotoState s (Rabbit r) =
    if s == r.state then
        idle $ Rabbit r
    else
        Rabbit $ r { state = s, timeInState = 0 }

walk :: forall m. MonadRandom m => Rabbit -> m Rabbit
walk (Rabbit r) = case r.state of
    Walking -> do
        dx <- range 2 4
        let
            sdx = case r.direction of
                Left -> -1
                Right -> 1
        dy <- do
            intended <- choose $ NonEmpty (-1) [0, 0, 0, 1]
            if intended == -1 && r.y <= 0 then
                pure 0
            else if intended == 1 && r.y >= 80 then
                pure 0
            else
                pure intended
        pure $ Rabbit $ r {
            timeInState = r.timeInState + 1,
            x = r.x + sdx * dx,
            y = r.y + dy
            }
    _ -> pure $ gotoState Walking $ Rabbit r

turn :: Rabbit -> Rabbit
turn (Rabbit r) = case r.direction of
    Left -> Rabbit $ r { direction = Right, timeInState = 0 }
    Right -> Rabbit $ r { direction = Left, timeInState = 0 }

tick :: forall m. MonadRandom m => Rabbit -> m Rabbit
tick (Rabbit r) = case r.state of
    Walking ->
        if texture (Rabbit r) == Up then do
            stop <- choose $ NonEmpty true [false, false, false]
            if stop then
                pure $ gotoState Standing $ Rabbit r
            else if r.x < 16 && r.direction == Left then
                pure $ gotoState Standing $ Rabbit r
            else if r.x > 104 && r.direction == Right then
                pure $ gotoState Standing $ Rabbit r
            else
                walk $ Rabbit r
        else
            walk $ Rabbit r
    Standing -> do
        cutoff <- range 0 19
        if cutoff < r.timeInState then do
            todo <- choose $ NonEmpty (gotoState Sitting) [
                turn,
                gotoState EarFlop,
                gotoState Walking
                ]
            pure $ todo $ Rabbit r
        else
            pure $ idle $ Rabbit r
    EarFlop -> do
        cutoff <- range 0 19
        if cutoff < r.timeInState then
            pure $ gotoState Standing $ Rabbit r
        else
            pure $ idle $ Rabbit r
    Sitting -> do
        cutoff <- range 0 19
        if cutoff < r.timeInState then do
            newState <- choose $ NonEmpty Standing [Eating, Eating]
            pure $ gotoState newState $ Rabbit r
        else
            pure $ idle $ Rabbit r
    Eating -> do
        cutoff <- range 0 49
        if cutoff < r.timeInState then
            pure $ gotoState Sitting $ Rabbit r
        else
            pure $ idle $ Rabbit r

new :: forall m. MonadRandom m => m Rabbit
new = do
    x <- range 20 91
    y <- range 16 61
    direction <- choose $ NonEmpty Left [Right]
    pure $ Rabbit {
        x,
        y,
        direction,
        timeInState : 0,
        state : Standing
        }

render :: forall m. String -> Rabbit -> H.ComponentHTML Unit () m
render source (Rabbit r) = HH.img [
    HP.src $ source <> show (texture $ Rabbit r) <> ".png",
    HP.style $ fold [
        "left: ",
        show r.x,
        "px; top: ",
        show r.y,
        "px; ",
        if r.direction == Left then
            "transform: scaleX(-1); "
        else
            ""
        ]
    ]

type Slot = H.Slot Query Void

data Query a = Update a

component :: forall m. MonadRandom m => String -> H.Component Query Rabbit Unit m
component source = H.mkComponent{
    initialState : new,
    render : render source,
    eval : H.mkEval $ H.defaultEval { handleAction = pure tick }
    }