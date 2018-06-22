type node('a) =
	| ExpNode(exp('a))
	| PatNode(pat('a))
	| ClauseNode(clause('a))

and exp('a) =
	| EVar('a, string)
	| EApp('a, exp('a), list(exp('a)))
	| ELam('a, list(pat('a)), exp('a))
	| ELet('a, list(clause('a)), exp('a))
	| EHole('a)
	| EHoleNE('a, exp('a))
	| ECursor(exp('a))

and pat('a) =
	| PVar('a, string)
	| PHole('a)
	| PHoleNE('a, pat('a))
	| PCursor(pat('a))

and clause('a) =
	| Def('a, list(pat('a)), exp('a))
  | CCursor(clause('a));


type context('a) =
	| CRoot

	  /* Expression Context */
	| CELam1( int, list(pat('a)), context('a), list(pat('a)), exp('a) )
	| CELam2( int, list(pat('a)), context('a))
	| CEApp1( int, context('a), list(exp('a)))
	| CEApp2( int, exp('a), list(exp('a)), context('a), list(exp('a)) )
	| CELet1( int, list(clause('a)), context('a), list(clause('a)), exp('a) )
	| CELet2( int, list(clause('a)), context('a) )
	| CEHoleNE1( int, context('a) )

    /* Pattern Context */
	| CPHoleNE1( int, context('a) )

	  /* Clause Context */
	| CDef1( int, list(pat('a)), context('a), list(pat('a)), exp('a) )
	| CDef2( int, list(pat('a)), context('a) );


type zipper('a) = (node('a), context('a))

type inode = node(int);
type iexp = exp(int);

type zipperMap = array(zipper(int));



/* Quick and dirty undecorated constructors*/
let evar 		 = (n) 		 => EVar((), n);
let eapp 		 = (y, xs) => EApp((), y, xs);
let elam 		 = (ps, e) => ELam((), ps, e);
let elet 		 = (cs, e) => ELet((), cs, e);
let ehole 	 = () 		 => EHole(());
let ehole_ne = (e) 		 => EHoleNE((), e);

let pvar 		 = (n) 		 => PVar((), n);
let phole 	 = () 		 => PHole(());
let phole_ne = (e) 		 => PHoleNE((), e);

/* Expression Iterators */
let rec emap = (f, x): exp('b) =>
	switch (x) {
	| EVar(a, n) 		 => EVar(f(a), n)
	| EApp(a, y, xs) => EApp(f(a), emap(f, y), List.map(emap(f), xs))
  | ELam(a, ps, e) => ELam(f(a), List.map(pmap(f), ps), emap(f, e))
  | ELet(a, cs, e) => ELet(f(a), List.map(cmap(f), cs), emap(f, e))
  | EHole(a) 			 => EHole(f(a))
  | EHoleNE(a, e)  => EHoleNE(f(a), emap(f, e))
	| ECursor(e)     => ECursor(emap(f, e))
	}

and pmap = (f, p): pat('b) =>
	switch (p) {
	| PVar(a, n)    => PVar(f(a), n)
	| PHole(a)      => PHole(f(a))
	| PHoleNE(a, p) => PHoleNE(f(a), pmap(f, p))
	| PCursor(p)    => PCursor(pmap(f, p))
	}

and cmap = (f, c): clause('b) =>
	switch (c) {
	| Def(a, ps, body) => Def(f(a), List.map(pmap(f), ps), emap(f, body))
	| CCursor(c) => CCursor(cmap(f, c))
	};



let emapi = (f: (int, 'a) => 'b, x: exp('a)): exp('b) => {
	let nextIndex = ref(0);
	
	let getIndex = (): int => {
		let index = nextIndex^;
		nextIndex := nextIndex^ + 1;
		index
	};

	let f' = (a: 'a): 'b => { 
		let i = getIndex();
		f(i, a)
	};

	emap(f', x)
};


let indexRoot = (root: node('a)): (int, inode) => {
	let nextIndex = ref(0);
	
	let getIndex = (): int => {
		let index = nextIndex^;
		nextIndex := nextIndex^ + 1;
		index
	};

	let node =
		switch (root) {
		| ExpNode(e) => ExpNode(emap( _ => getIndex(), e))
		| PatNode(p) => PatNode(pmap( _ => getIndex(), p))
		| ClauseNode(c) => ClauseNode(cmap( _ => getIndex(), c))
		};

	let n = nextIndex^;
	(n, node)
};


let rec getNodeIndex = (node: inode): int =>
	switch (node) {
	| ExpNode(e) =>
			switch (e) {
			| EVar(vId, _) => vId
			| EApp(vId, _, _) => vId
			| ELam(vId, _, _) => vId
			| ELet(vId, _, _) => vId
			| EHole(vId) => vId
			| EHoleNE(vId, _) => vId
			| ECursor(e) => getNodeIndex(ExpNode(e))
			}
	| PatNode(p) =>
			switch (p) {
			| PVar(vId, _) => vId
			| PHole(vId) => vId
			| PHoleNE(vId, _) => vId
			| PCursor(p) => getNodeIndex(PatNode(p))
			}

	| ClauseNode(c) =>
			switch (c) {
			| Def(vId, _, _) => vId
			| CCursor(c) => getNodeIndex(ClauseNode(c))
			}
	};


let isCursor = (node) =>
	switch (node) {
	| ExpNode(ECursor(_)) => true
	| PatNode(PCursor(_)) => true
	| ClauseNode(CCursor(_)) => true
	| _ => false
	};

let makeCursor = (node) =>
	switch (node) {
	| ExpNode(e) => ExpNode(ECursor(e))
	| PatNode(p) => PatNode(PCursor(p))
	| ClauseNode(c) => ClauseNode(CCursor(c))
	};

let delCursor = (node) =>
	switch (node) {
	| ExpNode(ECursor(e)) => ExpNode(e)
	| PatNode(PCursor(p)) => PatNode(p)
	| ClauseNode(CCursor(c)) => ClauseNode(c)
	};


let zipr = (f: (list('a), 'a, list('a), 'b) => 'b, xs: list('a), acc: 'b): 'b =>
{
	let rec step = (sx, x, xs, acc) =>
		switch (xs) {
		| [] => f(sx, x, xs, acc)
		| [x', ...xs'] => 
				let acc' = f(sx, x, xs, acc); 
				step([x, ...sx], x', xs', acc')
		};

	switch(xs) {
	| [] => acc
	| [x, ...xs] => step([], x, xs, acc)
	}
};


let buildContextMap = (root: inode, n: int): zipperMap =>
{
	let ctxmap = Array.make(n, (ExpNode(EHole(-1)), CRoot));
	let q = Queue.create();
	Queue.add( (root, CRoot), q );

	while (! Queue.is_empty(q)) {
		let (node, ctx) = Queue.take(q);
		let vId = getNodeIndex(node);

		if (! isCursor(node)) { ctxmap[vId] = (makeCursor(node), ctx) };

		switch (node) {
		| ExpNode(e) =>
				switch (e) {
				| EVar(vId, n) => ()
				| EApp(vId, f, xs) =>
						let node1 = ExpNode(f);
						let node2(x) = ExpNode(x);
						let ctx1 =  CEApp1(vId, ctx, xs);
						let ctx2(l, r) = CEApp2(vId, f, l, ctx, r );
						let z = (node1, ctx1); 
						let zs = zipr( (l, x, r, zs) => [(node2(x), ctx2(l, r)), ...zs]
												 , xs, []);
						
						List.iter((z => Queue.add(z, q)), [z, ...zs])

				| ELam(vId, ps, a) =>
						let node1(p) = PatNode(p);
						let node2 = ExpNode(a);
						let ctx1(l, r) =  CELam1(vId, l, ctx, r, a);
						let ctx2 = CELam2(vId, ps, ctx);

						let pzs = zipr( (l, p, r, pzs) => [(node1(p), ctx1(l, r)), ...pzs]
												  , ps, []);
						let az = (node2, ctx2); 
						
						List.iter((pz => Queue.add(pz, q)), pzs);
						Queue.add(az, q)

				| ELet(vId, cs, a) =>
						let node1(c) = ClauseNode(c);
						let node2 = ExpNode(a);
						let ctx1(l, r) =  CELet1(vId, l, ctx, r, a);
						let ctx2 = CELet2(vId, cs, ctx);

						let czs = zipr( (l, c, r, czs) => [(node1(c), ctx1(l, r)), ...czs]
												  , cs, []);
						let az = (node2, ctx2); 
						
						List.iter((cz => Queue.add(cz, q)), czs);
						Queue.add(az, q)

				| EHoleNE(vId, x) =>
						Queue.add((ExpNode(x), CEHoleNE1(vId, ctx)), q)
				| ECursor(e) =>
						Queue.add((ExpNode(e), ctx), q)
				}

		| PatNode(p) =>
				switch (p) {
				| PVar(vId, n) => ()
				| PHole(vId) => ()
				| PHoleNE(vId, p') =>
						Queue.add((PatNode(p'), CEHoleNE1(vId, ctx)), q)
				| PCursor(p) =>
						Queue.add((PatNode(p), ctx), q)
				}

		| ClauseNode(c) =>
				switch (c) {
				| Def(vId, ps, e) =>
						let node1(p) = PatNode(p);
						let node2 = ExpNode(e);
						let ctx1(l, r) =  CDef1(vId, l, ctx, r, e);
						let ctx2 = CDef2(vId, ps, ctx);

						let pzs = zipr( (l, p, r, pzs) => [(node1(p), ctx1(l, r)), ...pzs]
												  , List.rev(ps), []);
						let az = (node2, ctx2); 
						
						List.iter((pz => Queue.add(pz, q)), pzs);
						Queue.add(az, q)
				
				| CCursor(c) =>
						Queue.add((ClauseNode(c), ctx), q)
				}
		};
	};

	ctxmap
};


let unzip = ((n, c): zipper('a)): node('a) => {
	let rec go = (n: node(int), c: context(int)): node(int) =>
		switch (c) {
		| CRoot => n
		
		| CELam1(id, l, c2, r, e) =>
				let PatNode(p) = n;
				let ps = List.(append(rev(l), [p, ...r]));
				go (ExpNode(ELam(id, ps, e)), c2)
		
		| CELam2(id, ps, c2) =>
				let ExpNode(e) = n;
				go (ExpNode(ELam(id, ps, e)), c2)
		
		| CEApp1(id, c2, args) =>
				let ExpNode(fn) = n;
				go (ExpNode(EApp(id, fn, args)), c2)
		
		| CEApp2(id, fn, l, c2, r) =>
				let ExpNode(arg) = n;
				let args = List.(append(rev(l), [arg, ...r]));
				go (ExpNode(EApp(id, fn, args)), c2)

		| CELet1(id, l, c2, r, body) =>
				let ClauseNode(cl) = n;
				let cs = List.(append(rev(l), [cl, ...r]));
				go (ExpNode(ELet(id, cs, body)), c2)
		
		| CELet2(id, cs, c2) =>
				let ExpNode(body) = n;
				go (ExpNode(ELet(id, cs, body)), c2)
		
		| CEHoleNE1(id, c2) =>
				let ExpNode(e) = n;
				go (ExpNode(EHoleNE(id, e)), c2)

		| CPHoleNE1(id, c2) =>
				let PatNode(p) = n;  
				go (PatNode(PHoleNE(id, p)), c2)

		| CDef1(id, l, c2, r, e) =>
				let PatNode(p) = n;  
				let ps = List.(append(rev(l), [p, ...r]));
				go (ClauseNode(Def(id, ps, e)), c2)

		| CDef2(id, ps, c2) =>
				let ExpNode(e) = n;  
				go (ClauseNode(Def(id, ps, e)), c2)
		};

	go(n, c);
};


let rec render = (node) =>
	switch (node) {
	| ExpNode(e) =>
			switch (e) {
			| EVar(id, n) => 
				<TextArea>
					{ReasonReact.string(n)}
				</TextArea>
			
			| EApp(id, f, xs) =>
				<TextArea>
					{ render(ExpNode(f)) }
					{ ReasonReact.string(" ") }
					{ ReasonReact.array(Array.of_list(List.map(x => render(ExpNode(x)), xs))) }
				</TextArea>
			
			| ELam(id, ps, e) => 
				<TextArea>
					{ ReasonReact.string("\\") }

					{ ReasonReact.array(Array.of_list(List.map(p => render(PatNode(p)), ps))) }

					{ ReasonReact.string(" -> ") }
					
					{ render(ExpNode(e)) }
				</TextArea> 
			
			| ELet(id, cs, e) =>
				<TextArea>
					{ ReasonReact.string("let ") }
					
					{ ReasonReact.array(Array.of_list(List.map(c => render(ClauseNode(c)), cs))) }
					
					{ ReasonReact.string(" in ") }
					
					{ render(ExpNode(e)) }
				</TextArea>
			
			| EHole(id) =>
				<TextArea> 
					{ReasonReact.string("_")}
				</TextArea>
			
			| EHoleNE(id, e) => 
				<TextArea>
					{ReasonReact.string("(|")}
						{ render(ExpNode(e)) }
					{ReasonReact.string("|)")}
				</TextArea>

			| ECursor(e) =>
				<Paper>
					{ render(ExpNode(e)) }
				</Paper>
			}
	| PatNode(p) =>
			switch (p) {
			| PVar(id, n) =>
				<TextArea>
					{ReasonReact.string(n)}
				</TextArea>

			| PHole(id) =>
				<TextArea>
					{ReasonReact.string("_")}
				</TextArea>
				
			| PHoleNE(id, p) =>
			  <TextArea>
					{ReasonReact.string("(|")}
						{ render(PatNode(p)) }
					{ReasonReact.string("|)")}
				</TextArea>

			| PCursor(p) =>
				<Paper>
					{ render(PatNode(p)) }
				</Paper>
			}
	| ClauseNode(c) =>
			switch (c) {
			| Def(id, ps, e) =>
				<TextArea>
					{ ReasonReact.array(Array.of_list(List.map(p => render(PatNode(p)), ps))) }

					{ ReasonReact.string(" = ") }
					
					{ render(ExpNode(e)) }

					<br />
				</TextArea> 
			
			| CCursor(c) =>
				<Paper>
					{ render(ClauseNode(c)) }
				</Paper>
			}
	}


let goDown = ((node,ctx): zipper('a)): zipper('a) => {
	let node = delCursor(node);
	let (node', ctx') =
		switch (node) {
		| ExpNode(e) =>
				switch (e) {
				| EVar(id, n) => (node, ctx)
				| EApp(id, f, xs) => (ExpNode(f), CEApp1(id, ctx, xs))
				| ELam(id, [], a) => (ExpNode(a), CELam2(id, [], ctx))
				| ELam(id, [p, ...ps], a) => (PatNode(p), CELam1(id, [], ctx, ps, a))
				| ELet(id, [], a) => (ExpNode(a), CELet2(id, [], ctx))
				| ELet(id, [c, ...cs], a) => (ClauseNode(c), CELet1(id, [], ctx, cs, a))
				| EHole(id) => (node, ctx)
				| EHoleNE(id, e1) => (ExpNode(e1), CEHoleNE1(id, ctx))
				}
		| PatNode(p) =>
				switch (p) {
				| PVar(id, n) => (node, ctx)
				| PHole(id) => (node, ctx)
				| PHoleNE(id, p) => (PatNode(p), CPHoleNE1(id, ctx))
				}
		| ClauseNode(c) =>
				switch (c) {
				| Def(id, [], e) => (ExpNode(e), CDef2(id, [], ctx))
				| Def(id, [p, ...ps], e) => (PatNode(p), CDef1(id, [], ctx, ps, e))
				}
		};
	(makeCursor(node'), ctx')
};

let goUp = ((node,c1): zipper('a)): zipper('a) => {
	let node = delCursor(node);
	let (node', ctx') =
		switch (c1) {
		| CRoot => (node, c1)
		| CELam1(id, l, c2, r, a) =>
				let PatNode(p) = node;
				let ps = List.rev(l) @ [p, ...r];
				let node' = ExpNode(ELam(id, ps, a));
				(node', c2)
		
		| CELam2(id, ps, c2) =>
				let ExpNode(a) = node;
				let node' = ExpNode(ELam(id, ps, a));
				(node', c2)

		| CEApp1(id, c2, xs) =>
				let ExpNode(f) = node;
				let node' = ExpNode(EApp(id, f, xs)); 
				(node', c2)

		| CEApp2(id, f, l, c2, r) =>
				let ExpNode(x) = node;
				let xs = List.rev(l) @ [x, ...r];
				let node' = ExpNode(EApp(id, f, xs));
				(node', c2)
				
		| CELet1(id, l, c2, r, a) =>
				let ClauseNode(c) = node;
				let cs = List.rev(l) @ [c, ...r];
				let node' = ExpNode(ELet(id,cs,a));
				(node', c2)

		| CELet2(id, cs, c2) =>
				let ExpNode(a) = node;
				let node' = ExpNode(ELet(id, cs, a));
				(node', c2)

		| CEHoleNE1(id, c2) =>
				let ExpNode(e) = node;
				let node' = ExpNode(EHoleNE(id, e));
				(node', c2)
		
		| CPHoleNE1(id, c2) =>
				let PatNode(p) = node;
				let node' = PatNode(PHoleNE(id, p));
				(node', c2)

		| CDef1(id, l, c2, r, body) =>
				let PatNode(p) = node;
				let ps = List.append(List.rev(l), [p, ...r]);
				let node' = ClauseNode(Def(id, ps, body));
				(node', c2)

		| CDef2(id, ps, c2) =>
				let ExpNode(body) = node;
				let node' = ClauseNode(Def(id, ps, body));
				(node', c2)
		};
	(makeCursor(node'), ctx')
};


let goLeft = ((node,ctx): zipper('a)): zipper('a) => {
	let node = delCursor(node);
	let (node', ctx') =
		switch (ctx) {
		| CRoot => (node, ctx)
		| CELam1(id, l, c2, r, a) => (node, ctx)
		| CELam1(id, [p2, ...l], c2, r, a) =>
				let PatNode(p1) = node;
				let r = [p1, ...r];
				let node' = PatNode(p2);
				let ctx' = CELam1(id, l, c2, r, a);
				(node', ctx')
		
		| CELam2(id, [], c2) => (node, ctx)
		| CELam2(id, ps, c2) =>
				let ExpNode(a) = node;
				let [p, ...l] = List.rev(ps);
				let r = [];
				let node' = PatNode(p);
				let ctx' = CELam1(id, l, c2, r, a);
				(node', ctx')

		| CEApp1(id, c2, xs) => (node, ctx)

		| CEApp2(id, f, [], c2, r) =>
				let ExpNode(x) = node;
				let xs = [x, ...r];
				let node' = ExpNode(f);
				let ctx' = CEApp1(id, c2, xs);
				(node', ctx')
		
		| CEApp2(id, f, [x2, ...l], c2, r) =>
				let ExpNode(x1) = node;
				let r = [x1, ...r];
				let node' = ExpNode(x2);
				let ctx' = CEApp2(id, f, l, c2, r);
				(node', ctx')
				
		| CELet1(id, [], c2, r, a) => (node, ctx)
		| CELet1(id, [cl2, ...l], c2, r, a) =>
				let ClauseNode(cl1) = node;
				let r = [cl1, ...r];
				let node' = ClauseNode(cl2);
				let ctx' = CELet1(id, l, c2, r, a);
				(node', ctx')

		| CELet2(id, [], c2) => (node, ctx)
		| CELet2(id, cls, c2) =>
				let ExpNode(a) = node;
				let [cl, ...l] = List.rev(cls); 
				let node' = ClauseNode(cl);
				let ctx' = CELet1(id, l, c2, [], a);
				(node', ctx')

		| CEHoleNE1(id, c2) => (node, ctx)
		| CPHoleNE1(id, c2) => (node, ctx)

		| CDef1(id, [], c2, r, body) => (node, ctx)
		| CDef1(id, [p2, ...l], c2, r, body) =>
				let PatNode(p1) = node;
				let r = [p1, ...r];
				let node' = PatNode(p2);
				let ctx' = CDef1(id, l, c2, r, body);
				(node', ctx')
		
		| CDef2(id, [], c2) => (node, ctx)
		| CDef2(id, ps, c2) =>
				let ExpNode(body) = node;
				let [p, ...l] = List.rev(ps);
				let node' = PatNode(p);
				let ctx' = CDef1(id, l, c2, [], body);
				(node', ctx')
		};
	(makeCursor(node'), ctx')
};

let goRight = ((node,ctx): zipper('a)): zipper('a) => {
	let node = delCursor(node);
	let (node', ctx') =
		switch (ctx) {
		| CRoot => (node, ctx)
		| CELam1(id, l, c2, [], a) =>
				let PatNode(p1) = node;
				let ps = List.rev([p1, ...l]);
				let node' = ExpNode(a);
				let ctx' = CELam2(id, ps, c2);
				(node', ctx');

		| CELam1(id, l, c2, [p2, ...r], a) =>
				let PatNode(p1) = node;
				let l = [p1, ...l];
				let node' = PatNode(p2);
				let ctx' = CELam1(id, l, c2, r, a);
				(node', ctx')
		
		| CELam2(id, ps, c2) => (node, ctx)

		| CEApp1(id, c2, []) => (node, ctx)
		| CEApp1(id, c2, [x, ...r]) => (node, ctx)
				let ExpNode(f) = node;
				let node' = ExpNode(x);
				let ctx' = CEApp2(id, f, [], c2, r);
				(node', ctx')

		| CEApp2(id, f, l, c2, []) => (node, ctx)
		
		| CEApp2(id, f, l, c2, [x2, ...r]) =>
				let ExpNode(x1) = node;
				let l = [x1, ...l];
				let node' = ExpNode(x2);
				let ctx' = CEApp2(id, f, l, c2, r);
				(node', ctx')

		| CELet1(id, l, c2, [], a) =>
			let ClauseNode(cl) = node;
			let cls = List.rev([cl, ...l]);
			let node' = ExpNode(a);
			let ctx' = CELet2(id, cls, c2);
			(node', ctx')

		| CELet1(id, l, c2, [cl2, ...r], a) =>
				let ClauseNode(cl1) = node;
				let l = [cl1, ...l];
				let node' = ClauseNode(cl2);
				let ctx' = CELet1(id, l, c2, r, a);
				(node', ctx')
 
		| CELet2(id, cls, c2) => (node, ctx)

		| CEHoleNE1(id, c2) => (node, ctx)
		| CPHoleNE1(id, c2) => (node, ctx)

		| CDef1(id, [], c2, r, body) => (node, ctx)
		| CDef1(id, [p2, ...l], c2, r, body) =>
				let PatNode(p1) = node;
				let r = [p1, ...r];
				let node' = PatNode(p2);
				let ctx' = CDef1(id, l, c2, r, body);
				(node', ctx')
		
		| CDef2(id, [], c2) => (node, ctx)
		| CDef2(id, ps, c2) =>
				let ExpNode(body) = node;
				let [p, ...l] = List.rev(ps);
				let node' = PatNode(p);
				let ctx' = CDef1(id, l, c2, [], body);
				(node', ctx')
		};
	(makeCursor(node'), ctx')
};


/*
type expGraph = {
	root: inode,
	size: int,
	nodes: array(inode),
	edges: array(array(int)),
	parents: array(int),
	positions: array(int)
};

let toGraph = (e: expRaw): expGraph => {
	/* Reverse arrays for graph */
	let (n, root) = indexRoot(ExpNode(e));

	let nodes = Array.make(n, ExpNode(ref(EHole(-1))))
	let parents = Array.make(n, -1);
	let positions = Array.make(n, -1);
		let edges = Array.make(n, []);
	
	let q = Queue.create();
	Queue.add((-1, -1, root), q);

	/* Determine parents thru bfs traversal */
	while(! Queue.is_empty(q)) {
		let freePos = ref(0);
		let getPos = () => { let pos = freePos^; freePos := freePos^ + 1; pos}

		let (pId, pos, node) = Queue.take(q);
		let vId = getNodeIndex(node);
		parents[vId] = pId;
		nodes[vId] = node;

		/* When node is non-root */
		if (pId > -1) {
			edges[pId] = [vId, ...edges[pId]];
			positions[vId] = pos;
		};

		switch (node) {
		| ExpNode(e) =>
				switch (e^) {
				| EVar(vId, n) => ()
				| EApp(vId, f, xs) =>
						Queue.add((pId, getPos(), ExpNode(f)), q);
						List.iter((e => Queue.add((pId, getPos(), ExpNode(e)), q)), xs)

				| ELam(vId, ps, a) =>
						List.iter((p => Queue.add((pId, getPos(), PatNode(p)), q)), ps);
						Queue.add((pId, getPos(), ExpNode(a)), q)

				| ELet(vId, bs, a) =>
						List.iter((b => Queue.add((pId, getPos(), ClauseNode(b)), q)), bs);
						Queue.add((pId, getPos(), ExpNode(a)), q)

				| EHole(vId) => ()
				
				| EHoleNE(vId, x) =>
						Queue.add((pId, getPos(), ExpNode(x)), q)
				}

		| PatNode(p) =>
				switch (p^) {
				| PVar(vId, n) => ()
				| PHole(vId) => ()
				| PHoleNE(vId, p') =>
						Queue.add((pId, getPos(), PatNode(p')), q)
				}

		| ClauseNode(c) =>
				switch (c^) {
				| Def(vId, ps, e) =>
						List.iter((p => Queue.add((pId, getPos(), PatNode(p)), q)), ps);
						Queue.add((pId, getPos(), ExpNode(e)), q)
				}
		};
	};

	let edges' = Array.map(children => Array.of_list(List.rev(children)), edges);

	{root: root, size: n, nodes: nodes, edges: edges', parents: parents, positions: positions}
};


let goUp = (vId: int, g: expGraph): int => {
	if (vId > 0) {
		g.parents[vId]
	} else {
		vId
	}
};


let goDown = (vId: int, g: expGraph): int => {
	let children = g.edges[vId];
	if(Array.length(children) > 0) {
		children[0]
	} else {
		vId
	}
};


let goLeft = (vId: int, g: expGraph): int => {
	if (vId > 0) {
		let pId = g.parents[vId];
		let nextPos = g.positions[vId] - 1;
		let children = g.edges[pId];
		if(0 <= nextPos && nextPos < Array.length(children))
		{
			g.edges[pId][nextPos]
		} else {
			vId
		}
	} else {
		vId
	}
};


let goRight = (vId: int, g: expGraph): int => {
	let pId = g.parents[vId];

	if (pId > -1) {
		let nextPos = g.positions[vId] + 1;
		let children = g.edges[pId];
		if(0 <= nextPos && nextPos < Array.length(children))
		{
			g.edges[pId][nextPos]
		} else {
			vId
		}
	} else {
		vId
	}
};
*/


/*
type zexp = 
	| EZVar(string)
	| EZApp(zexp, zexp)
	| EZLam(string, zexp)
	| EZLet(string, zexp, zexp)
	| EZHole
	| EZHoleNE(zexp)
	| EZ(zexp);



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


let zipE = (e: exp): ezip => (e, CRoot);

let expzipGet = ((e, _): ezip): exp => e;






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

*/