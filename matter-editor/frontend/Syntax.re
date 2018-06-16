type exp =
	| EVar(string)
	| EApp(exp, list(exp))
	| ELam(list(pat), exp)
	| ELet(list(letbind), exp)
	| EHole
	| EHoleNE(exp)

and pat =
	| PVar(string)
	| PHole
	| PHoleNE(pat)

and letbind =
	| LetBind(pat, exp);


type node =
	| ExpNode(exp)
	| PatNode(pat)
	| BindNode(letbind);


type zexp = 
	| EZVar(string)
	| EZApp(zexp, zexp)
	| EZLam(string, zexp)
	| EZLet(string, zexp, zexp)
	| EZHole
	| EZHoleNE(zexp)
	| EZ(zexp);



type expGraph = {
	edges: array(list(int)),
	parents: array(int),
	labels: array(expLabel)
}

and expLabel =
	| LVar(string)
	| LApp
	| LLam
	| LLet
	| LHole
	| LHoleNE;


let rec sumInts = (ls: list(int)): int =>
	switch(ls) {
	| [] => 0
	| [x, ...xs] => x + sumInts(xs)
	};


let rec countExps = (e: exp): int =>
	switch (e) {
	| EVar(_) => 1
	| EApp(f, xs) =>
			1 + countExps(f)
				+ sumInts(List.map(countExps, xs))
	
	| ELam(ps, x) =>
			1 + sumInts(List.map(countPats, ps))
			  + countExps(x)
	
	| ELet(bs, x) =>
			1 + sumInts(List.map(countLetBinds, bs))
				+ countExps(x)
	
	| EHole => 1
	
	| EHoleNE(x) => 1 + countExps(x)
	}

and countPats = (p: pat): int =>
	switch (p) {
	| PVar(_) => 1
	| PHole => 1
	| PHoleNE(p1) => 1 + countPats(p1)
	}

and countLetBinds = (b: letbind): int =>
	switch (b) {
	| LetBind(p, e) => 1 + countPats(p) + countExps(e)
	};


let toGraph = (e: exp): expGraph => {
	/* Reverse arrays for graph */
	let n = countExps(e);
	let edges = Array.make(n, []);
	let parents = Array.make(n, -1);
	let labels = Array.make(n, LHole);

	/* Traverse expression, generating graph */

	let q = Queue.create();
	Queue.add((-1, ExpNode(e)), q);

	let vId = ref(0);

	while(! Queue.is_empty(q)) {
		let (pId, e) = Queue.take(q);
		switch (e) {
		| ExpNode(EVar(n)) =>
				Array.set(labels, vId^, LVar(n));
				Array.set(parents, vId^, pId)

		| ExpNode(EApp(f, xs)) =>
				Array.set(labels, vId^, LApp);
			  Queue.add((pId, ExpNode(f)), q);
				List.iter((e => Queue.add((pId, ExpNode(e)), q)), xs)

		| ExpNode(ELam(ps, a)) =>
				Array.set(labels, vId^, LLam);
				List.iter((p => Queue.add((pId, PatNode(p)), q)), ps);
			  Queue.add((pId, ExpNode(a)), q)

		| ExpNode(ELet(bs, a)) =>
				Array.set(labels, vId^, LLet);
				List.iter((b => Queue.add((pId, BindNode(b)), q)), bs);
				Queue.add((pId, ExpNode(a)), q)

		| ExpNode(EHole) =>
				Array.set(labels, vId^, LHole)
		
		| ExpNode(EHoleNE(x)) =>
				Array.set(labels, vId^, LHoleNE);
				Queue.add((pId, ExpNode(x)), q)
		};

		Array.set(parents, vId^, pId);
		vId := vId^ + 1;
	};


	{edges: edges, parents: parents, labels:labels}
};


type cexp =
	| CRoot
	| CLam1(string, cexp)
	| CApp1(cexp, exp)
	| CApp2(exp, cexp)
	| CLet1(string, cexp, exp)
	| CLet2(string, exp, cexp)
	| CHoleNE1(cexp);


type ezip = (exp, cexp);


let rec toZExp = (e: exp): zexp =>
	switch(e) {
	| EVar(n) => EZVar(n)
	| EApp(e1, e2) => EZApp(toZExp(e1), toZExp(e2))
	| ELam(n, e) => EZLam(n, toZExp(e))
	| ELet(n, e1, e2) => EZLet(n, toZExp(e1), toZExp(e2))
	| EHole => EZHole
	| EHoleNE(e2) => EZHoleNE(toZExp(e2))
	}


let rec fromZExp = (e: zexp): exp =>
	switch(e) {
	| EZVar(n) => EVar(n)
	| EZApp(e1, e2) => EApp(fromZExp(e1), fromZExp(e2))
	| EZLam(n, e) => ELam(n, fromZExp(e))
	| EZLet(n, e1, e2) => ELet(n, fromZExp(e1), fromZExp(e2))
	| EZHole => EHole
	| EZHoleNE(e2) => EHoleNE(fromZExp(e2))
	| EZ(e) => fromZExp(e)
	}



let goDown = ((e,c): ezip): ezip =>
	switch (e) {
	| EVar(n) => (e,c)
	| EApp(e1, e2) => (e1, CApp1(c, e2))
	| ELam(n, e1) => (e1, CLam1(n, c))
	| ELet(n, e1, e2) => (e1, CLet1(n, c, e2))
	| EHoleNE(e1) => (e1, CHoleNE1(c))
	};

let goUp = ((e,c1): ezip): ezip =>
	switch (c1) {
	| CRoot => (e, c1)
	| CLam1(n, c2) => (ELam(n, e), c2)
	| CApp1(c2, e2) => (EApp(e, e2), c2)
	| CApp2(e1, c2) => (EApp(e1, e), c2)
	| CLet1(n, c2, e2) => (ELet(n,e,e2), c2)
	| CLet2(n, e1, c2) => (ELet(n,e1,e), c2)
	| CHoleNE1(c2) => (EHoleNE(e), c2);
	};


let goLeft = ((e,c): ezip): ezip =>
	switch (c) {
	| CRoot => (e, c)
	| CLam1(_,_) => (e, c)
	| CApp1(_,_) => (e, c)
	| CApp2(e1, c) => (e1, CApp1(c, e))
	| CLet1(_,_,_) => (e, c)
	| CLet2(n, e1, c) => (e1, CLet1(n, c, e))
	| CHoleNE1(_) => (e, c);
	};


let goRight = ((e,c): ezip): ezip =>
	switch (c) {
	| CRoot => (e, c)
	| CLam1(_,_) => (e, c)
	| CApp1(c,e2) => (e2, CApp2(e,c))
	| CApp2(_,_) => (e, c)
	| CLet1(n, c, e2) => (e2, CLet2(n, e, c))
	| CLet2(_,_,_) => (e, c)
	| CHoleNE1(_) => (e, c);
	};



let zipE = (e: exp): ezip => (e, CRoot);

let expzipGet = ((e, _): ezip): exp => e;




let rec unzipE = ((e, c): ezip): exp => {
	let rec go = (e: exp, c: cexp): exp =>
		switch (c) {
		| CRoot => e
		| CLam1(n,  c2) => go (ELam(n, e), c2)
		| CApp1(c2, e2) => go (EApp(e, e2), c2)
		| CApp2(e1, c2) => go (EApp(e1, e), c2)
		| CLet1(n, c2, e2) => go (ELet(n, e, e2), c2)
		| CLet2(n, e1, c2) => go (ELet(n, e1, e), c2)
		| CHoleNE1(c2) => go (EHoleNE(e), c2);
		};

	go(e, c);
};

let rec unzipZ = ((e, c): ezip): zexp => {
	let rec go = (e: zexp, c: cexp): zexp =>
		switch (c) {
		| CRoot => e
		| CLam1(n,  c2) => go (EZLam(n, e), c2)
		| CApp1(c2, e2) => go (EZApp(e, toZExp(e2)), c2)
		| CApp2(e1, c2) => go (EZApp(toZExp(e1), e), c2)
		| CLet1(n, c2, e2) => go (EZLet(n, e, toZExp(e2)), c2)
		| CLet2(n, e1, c2) => go (EZLet(n, toZExp(e1), e), c2)
		| CHoleNE1(c2) => go (EZHoleNE(e), c2);
		};

	go(EZ(toZExp(e)), c);
};


let zipZ = (e: zexp): ezip => {
	let rec maybeContext = (e: zexp, c: cexp): option(ezip) =>
		switch (e) { 
		| EZVar(n) => None
		| EZApp(e1, e2) =>
			switch (maybeContext(e1, CApp1(c, fromZExp(e2)))) {
			| Some(z) => Some(z)
			| None =>
				switch (maybeContext(e2, CApp2(fromZExp(e1), c))) {
				| Some(z) => Some(z)
				| None => None
				}
			}
		
		| EZLam(n, e1) =>
			switch (maybeContext(e1, CLam1(n, c))) {
			| Some(z) => Some(z)
			| None => None
			}
		
		| EZLet(n, e1, e2) =>
			switch (maybeContext(e1, CLet1(n, c, fromZExp(e2)))) {
			| Some(z) => Some(z)
			| None =>
				switch (maybeContext(e2, CLet2(n, fromZExp(e1), c))) {
				| Some(z) => Some(z)
				| None => None
				}
			}
	
		| EZHole => None
	
		| EZHoleNE(e1) =>
			switch (maybeContext(e1, CHoleNE1(c))) {
			| Some(z) => Some(z)
			| None => None
			}
	
		| EZ(e) => Some((fromZExp(e), c))
		};

	let mctx = maybeContext(e, CRoot);
	switch (mctx) {
	| Some((e, c)) => (e, c)
	| None => (fromZExp(e), CRoot)
	};
};







let rec render = (ez) =>
	switch (ez) {
	| EZVar(n) => 
		<TextArea>
			{ReasonReact.string(n)}
		</TextArea>
	
	| EZApp(e1, e2) =>
		<TextArea>
			{render(e1)}
			{ReasonReact.string(" ")}
			{render(e2)}
		</TextArea>
	
	| EZLam(n, e) => 
		<TextArea>
			{ReasonReact.string("\\")} 
			
			<TextArea>
				{ReasonReact.string(n)}
			</TextArea>

			{ReasonReact.string(" -> ")}
			
			{render(e)}
		</TextArea> 
	
	| EZLet(n, v, e) =>
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
	
	| EZHole =>
		<TextArea> 
			{ReasonReact.string("_")}
		</TextArea>
	
	| EZHoleNE(e) => 
		<TextArea>
			{ReasonReact.string("(|")}
				{render(e)}
			{ReasonReact.string("|)")}
		</TextArea>
	| EZ(e) =>
		<Fade>
			<Paper>
				{render(e)}
			</Paper>
		</Fade>
	}