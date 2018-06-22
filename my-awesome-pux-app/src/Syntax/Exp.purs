module Syntax.Exp where

import Data.Functor
import Data.Traversable
import Data.Unit (Unit)

data ExpL = ExpL
data PatL = PatL


data Node a n
	= ExpNode (Exp a n)
	| PatNode (Pat a n)
	| DefNode (Def a n)

data Exp a n
	= EVar a n
	| EApp a (Exp a n) (Array (Exp a n))
	| ELam a (Array (Exp a n)) (Exp a n)
	| ELet a (Array (Exp a n)) (Exp a n)
	| EHole a
	| EHoleNE a (Exp a n)
	| ECursor (Exp a n)

data Pat a n
	= PVar a n
	| PHole a
	| PHoleNE a (Exp a n)
	| PCursor (Exp a n)

data Def a n
	= Def a (Array (Exp a n)) (Exp a n)
	| DCursor (Def a n)


-- The derivative (or context) for Node
data Node' a n =
	-- Root Context
	  CRoot

	-- Expression Context
	| CELam1 Int (Array (Pat a n)) (Node' a n) (Array (Pat a n)) (Exp a n)
	| CELam2 Int (Array (Pat a n)) (Node' a n)
	| CEApp1 Int (Node' a n) (Array (Exp a n))
	| CEApp2 Int (Exp a n) (Array (Exp a n)) (Node' a n) (Array (Exp a n))
	| CELet1 Int (Array (Def a n)) (Node' a n) (Array (Def a n)) (Exp a n)
	| CELet2 Int (Array (Def a n)) (Node' a n)
	| CEHoleNE1 Int (Node' a n)

  -- Pattern Context
	| CPHoleNE1 Int (Node' a n)

	-- Def Context
	| CDef1 Int (Array (Pat a n)) (Node' a n) (Array (Pat a n)) (Exp a n)
	| CDef2 Int (Array (Pat a n)) (Node' a n)