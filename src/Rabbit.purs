module Rabbit () where

data State = Standing | Sitting | Eating | EarFlop | Walking
	deriving (Eq)

data Direction = Left | Right
	deriving (Eq)

data Texture = BackLeg | Down | FrontLeg | Nom | Up | Ear

data Rabbit = Rabbit {
	state :: State,
	direction :: Direction,
	x :: Int,
	y :: Int,
	timeInState :: Int,
	}

texture :: Rabbit -> Texture
texture (Rabbit r) = case r.state of
	Standing -> Up
	Sitting -> Down
	Eating -> case r.timeInState `mod` 2 of
		0 -> Down
		1 -> Nom
	EarFlop -> Ear
	Walking -> case r.timeInState `mod` 3 of
		0 -> Up
		1 -> FrontLeg
		2 -> BackLeg

