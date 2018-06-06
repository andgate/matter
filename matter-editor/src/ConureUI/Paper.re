open Css;

let paperStyle = style([
			display(flexBox),
			flexDirection(column), 
			alignItems(stretch),
			backgroundColor(white),
			boxShadow(~y=px(3), ~blur=px(5), rgba(0, 0, 0, 0.3))
		]);

/* still in Greeting.re */
let component = ReasonReact.statelessComponent("Paper");

let make = (_children) => {
	...component, /* spread the template's other defaults into here  */
	render: _self => <div className=paperStyle> {ReasonReact.string("I'm a custom paper css widget!")} </div>
};