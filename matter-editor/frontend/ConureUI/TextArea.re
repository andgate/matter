open Css;

let textStyle = style([
			display(flexBox),
			flex(1),
			flexGrow(1),
			flexBasis(auto)
		]);

/* still in Greeting.re */
let component = ReasonReact.statelessComponent("TextArea");

let make = (children) => {
	...component,
	render: _self =>
		ReasonReact.createDomElement("div", ~props={"className": textStyle}, children)
};