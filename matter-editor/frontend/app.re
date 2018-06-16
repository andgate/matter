type state = {
  modules: option(array(ModuleData.module_)),
  currExp: Syntax.ezip
};

type action =
  | Loaded(array(ModuleData.module_))
  | MoveCursor(Action.dir)
  | KeyDown(int);


let exampleExp = 
  Syntax.(
    ELam
    ( "x"
    , EApp
      ( EVar("f")
      , EVar("x")
      )
    )
  );

let component = ReasonReact.reducerComponent("App");


let make = _children => {
  ...component,

  initialState: (): state => {
    modules: None,
    currExp: Syntax.zipE(exampleExp)
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
          modules: Some(loadedModules),
          currExp: state.currExp
        })
      | KeyDown(38) => ReasonReact.Update({
          modules: state.modules,
          currExp: Syntax.goUp(state.currExp)
        })
      
      | KeyDown(40) => ReasonReact.Update({
          modules: state.modules,
          currExp: Syntax.goDown(state.currExp)
        })
      
      | KeyDown(37) => ReasonReact.Update({
          modules: state.modules,
          currExp: Syntax.goLeft(state.currExp)
        })
      
      | KeyDown(39) => ReasonReact.Update({
          modules: state.modules,
          currExp: Syntax.goRight(state.currExp)
        })
    };
  },
  
  render: (self) => <ExprView zexp=Syntax.unzipZ(self.state.currExp) onKeyDown=( event => self.send(KeyDown( ReactEventRe.Keyboard.which(event) )) ) />,
};