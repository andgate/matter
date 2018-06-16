type action =
	| Move(dir)
	| Construct(Syntax.exp)
	| Del

and dir = 
	| Parent
	| Child
	| CrossLeft
	| CrossRight;

/*
let handleAction = (a: action, rootE: Syntax.exp) =>
{
} */