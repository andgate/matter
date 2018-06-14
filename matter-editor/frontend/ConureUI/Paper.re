open Css;

let paperStyle = style([
			display(flexBox),
			backgroundColor(white),
			boxShadow(~y=px(3), ~blur=px(5), rgba(0, 0, 0, 0.3))
		]);

/* still in Greeting.re */
let component = ReasonReact.statelessComponent("Paper");

let make = (children) => {
	...component,
	render: _self =>
		ReasonReact.createDomElement("div", ~props={"className": paperStyle}, children)
};