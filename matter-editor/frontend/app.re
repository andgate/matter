type state = {
  modules: option(array(ModuleData.module_)),
  root: Syntax.inode,
  ctxmap: Syntax.zipperMap,
  cursor: int
};

type action =
  | Loaded(array(ModuleData.module_))
  | MoveCursor(Action.dir)
  | KeyDown(int);


let exampleExp = 
  Syntax.(
    elam(
      [pvar("x")]
    , eapp(
        evar("f")
      , [evar("x"), evar("y"), evar("z")]
      )
    )
  );

let component = ReasonReact.reducerComponent("App");


let make = _children => {
  ...component,

  initialState: (): state => {
    let (n, root) = Syntax.(indexRoot(ExpNode(ECursor(exampleExp))));
    let ctxmap = Syntax.buildContextMap(root, n);
    let rootId = 0;
    let (root', _) = ctxmap[rootId];

    { modules: None,
      root: root',
      ctxmap: ctxmap,
      cursor: rootId
    }
  },

  didMount: self => {
    let handleModulesLoaded = (modsData) => self.send(Loaded(modsData));

    ModuleData.fetchModules()
      |> Js.Promise.then_( modsData => {
          handleModulesLoaded(modsData);
          Js.Promise.resolve();
        })
      |> ignore;
  },

  reducer: (action, state) => {
    switch action {
      | Loaded(loadedModules) => ReasonReact.Update({
          ...state, modules: Some(loadedModules)
        })
      
      | KeyDown(37) =>  /* Move Left */
          let z = Syntax.goLeft(state.ctxmap[state.cursor]);
          let (node, ctx) = z;
          let root = Syntax.unzip(z);
          ReasonReact.Update({
            ...state
          , root: root
          , cursor: Syntax.getNodeIndex(node)
          })

      | KeyDown(38) => /* Move Up */
          let z = Syntax.goUp(state.ctxmap[state.cursor]);
          let (node, ctx) = z;
          let root = Syntax.unzip(z);
          ReasonReact.Update({
            ...state
          , root: root
          , cursor: Syntax.getNodeIndex(node)
          })
      
      | KeyDown(39) =>  /* Move Right */
          let z = Syntax.goRight(state.ctxmap[state.cursor]);
          let (node, ctx) = z;
          let root = Syntax.unzip(z);
          ReasonReact.Update({
            ...state
          , root: root
          , cursor: Syntax.getNodeIndex(node)
          })

      | KeyDown(40) =>  /* Move Down */
          let z = Syntax.goDown(state.ctxmap[state.cursor]);
          let (node, ctx) = z;
          let root = Syntax.unzip(z);
          ReasonReact.Update({
            ...state
          , root: root
          , cursor: Syntax.getNodeIndex(node)
          })
    };
  },
  
  render: (self) => <ExprView root=self.state.root cursor=self.state.cursor onKeyDown=( event => self.send(KeyDown( ReactEventRe.Keyboard.which(event) )) ) />,
};