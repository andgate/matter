module Syntax.Zipper where

import Syntax.Exp

type Zipper a n = Tuple (Node a n) (Node' a n)

type Partition a n = Map Int (Zipper a n)

type Editor a n = Tuple Int (Partition a n)

{-



data TPossible a =
	TPossible
		{ leftward :: a
		, rightward :: a
		, inward :: a
		, outward :: a
		}

data TChoice = L | R | I | O


instance functorTPossible :: Functor TPossible where
	map f (TPossible d) = 
		TPossible
			{ leftward: f d.leftward
			, rightward: f d.rightward
			, inward: f d.inward
			, outward: f d.outward
			}

instance distributiveTPossible :: Distributive TPossible where
	distribute :: forall f a. (Functor f) => f (TPossible a) -> TPossible (f a)
	distribute fga =
		TPossible
			{ leftward: map (\(TPossible r) -> r.leftward) fga
			, rightward: map (\(TPossible r) -> r.rightward) fga
			, inward: map (\(TPossible r) -> r.inward) fga
			, outward: map (\(TPossible r) -> r.outward) fga
			}

	collect f = distribute <<< map f


instance representableTPossible :: Representable TPossible TChoice where
	index :: forall a. TPossible a -> TChoice -> a
	index (TPossible tp) L = tp.leftward
	index (TPossible tp) R = tp.rightward
	index (TPossible tp) I = tp.inward
	index (TPossible tp) O = tp.outward
	
	tabulate :: forall a. (TChoice -> a) -> TPossible a
	tabulate describe =
		TPossible
			{ leftward: describe L
			, rightward: describe R
			, inward: describe I
			, outward: describe O
			}



type Zipper a = Cofree TPossible a

-- Use tabulate to build our zpper
-- Use index to reap[]

moveTo :: forall a. Seq TChoice -> Zipper a -> Zipper a
moveTo ind = extend (\cfr -> index cfr ind)
-}