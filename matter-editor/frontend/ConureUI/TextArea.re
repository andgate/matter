open Css;

let textStyle = style([
			display(flexBox),
			backgroundColor(white),
		]);

/* still in Greeting.re */
let component = ReasonReact.statelessComponent("TextArea");

let make = (children) => {
	...component,
	render: _self =>
		ReasonReact.createDomElement("div", ~props={"className": textStyle}, children)
};