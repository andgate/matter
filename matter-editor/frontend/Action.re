type action =
	| Move(dir)
	| Construct(Syntax.exp)
	| Del
	| Finish

and dir = 
	| Parent
	| Child
	| CrossLeft
	| CrossRight;
