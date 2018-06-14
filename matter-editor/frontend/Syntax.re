type exp =
	| EVar(string)
	| EApp(exp, exp)
	| ELam(string, exp)
	| ELet(string, exp, exp)
	| Hole
	| NonEmptyHole(exp)
	| EZip(string, exp);


let rec render = (exp) =>
	switch (exp) {
	| EVar(n) => 
		<TextArea>
			{ReasonReact.string(n)}
		</TextArea>
	
	| EApp(e1, e2) =>
		<TextArea>
			{render(e1)}
			{ReasonReact.string(" ")}
			{render(e2)}
		</TextArea>
	
	| ELam(n, e) => 
		<TextArea>
			{ReasonReact.string("\\")} 
			
			<TextArea>
				{ReasonReact.string(n)}
			</TextArea>

			{ReasonReact.string(" -> ")}
			
			{render(e)}
		</TextArea> 
	
	| ELet(n, v, e) =>
		<TextArea>
			{ReasonReact.string("let ")}
			
			<TextArea>
				<TextArea>
					{ReasonReact.string(n)}
				</TextArea>

				{ReasonReact.string(" = ")}
			
				{render(v)}
			</TextArea>
			
			{ReasonReact.string(" in ")}
			
			{render(e)}
		</TextArea>
	
	| Hole =>
		<TextArea> 
			{ReasonReact.string("_")}
		</TextArea>
	
	| NonEmptyHole(e) => 
		<TextArea>
			{ReasonReact.string("(|")}
				{render(e)}
			{ReasonReact.string("|)")}
		</TextArea>

	| EZip(n, e) => 
		<Paper>
			{render(e)}
		</Paper>

	}
