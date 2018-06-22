type action =
	| Move(dir)
	| Construct(Syntax.inode)
	| Del

and dir = 
	| Parent
	| Child
	| SiblingLeft
	| SiblingRight;

/*
let handleAction = (a: action, rootE: Syntax.exp) =>
{
} */