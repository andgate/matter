let component = ReasonReact.statelessComponent("UserItem");

let make = (~user: UserData.user, _children) =>
{
	...component,
	
	render: _self =>
		<div className="UserItem">
			<p>{ReasonReact.string(user.firstName ++ " " ++ user.lastName)}</p>
		</div>
};