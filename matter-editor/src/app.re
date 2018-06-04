type state = {
  users: array(UserData.user)
};

type action =
  | Loaded(array(UserData.user));


let component = ReasonReact.reducerComponent("App");


let make = _children => {
  ...component,

  initialState: (): state => {
    users: [||]
  },

  didMount: self => {
    let handleUsersLoaded = (usersData) => self.send(Loaded(usersData));

    UserData.fetchUsers()
      |> Js.Promise.then_( userData => {
          handleUsersLoaded(userData);
          Js.Promise.resolve();
        })
      |> ignore;
  },

  reducer: (action, state) => {
    switch action {
      | Loaded(loadedUsers) => ReasonReact.Update({
          users: loadedUsers
        })
    };
  },
  
  render: (self) => {
    let userItems =
          Array.map(
            (user: UserData.user) => <UserItem key=user.firstName user=user />,
            self.state.users
          );

    <div>
      <h1> (ReasonReact.string("Users")) </h1>
      (ReasonReact.array(userItems))
    </div>;
  },
};