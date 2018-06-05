type state = {
  modules: option(array(ModuleData.module_))
};

type action =
  | Loaded(array(ModuleData.module_));


let component = ReasonReact.reducerComponent("App");


let make = _children => {
  ...component,

  initialState: (): state => {
    modules: None
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
          modules: Some(loadedModules)
        })
    };
  },
  
  render: (self) => {
    switch (self.state.modules) {
    | None     => <p>(ReasonReact.string("Loading..."))</p>
    | Some(ms) => {
        let moduleItems = Array.map(
            (m: ModuleData.module_) => <ModuleItem modl=m />,
            ms
          );

        <div>
          <h1> (ReasonReact.string("Modules")) </h1>
          (ReasonReact.array(moduleItems))
        </div>;
      }
    };
    
  },
};