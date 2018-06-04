type state = {
  users: array(UserData.user)
};

type action =
  | Loaded(UserData.user);


let component = ReasonReact.reducerComponent("App");


let make = _children => {
  ...component,

  initialState: (): state => {
    userData: [||]
  },

  didMount: self => {
    let handleUserLoaded = (userData) => self.send(Loaded(userData));

    UserData.fetchUsers()
      |> Js.Promise.then_( userData => {
          handleUserLoaded(userData);
          Js.Promise.resolve();
        })
      |> ignore;
  },

  reducer: (action, state) => {
    switch action {
      | Loaded(loadedUser) => ReasonReact.Update({
          userData: Array.append(state.userData, [|loadedUser|])
        })
    };
  },
  
  render: (self) => {
    let userItem =
          Array.map(
            (user: UserData.user) => <UserItem key=user.firstName user=user />,
            self.state.users
          );

    <div>
      <h1> (ReasonReact.string("Users")) </h1>
      {userItem}
    </div>;
  },
};