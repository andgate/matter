type user = {
	id: int,
	firstName: string,
	lastName: string
};

let parseUserJson = (json: Js.Json.t): user =>
	Json.Decode.{
		id: field("userId", int, json),
		firstName: field("userFirstName", string, json),
		lastName: field("userLastName", string, json),
	};

let parseUsersJson = (json): array(user) =>
  Json.parseOrRaise(json) |> Json.Decode.(array(parseUserJson));


let usersUrl = "http://127.0.0.1:8080/users";

let fetchUsers = () =>
  Js.Promise.(
    Axios.get(usersUrl)
      |> then_(response => {
          Js.log(response##data);
          resolve(response);
        })
      |> then_(response => resolve(Array.map(parseUserJson, response##data)))
  );