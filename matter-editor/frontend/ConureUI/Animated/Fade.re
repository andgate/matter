open Css;

let clearStyle: string = style([
    opacity(0.0),
    transform(`translate3d(px(-100), px(0), px(0))),
    transition(~duration=100, ~delay=1, ~timingFunction=`ease, "opacity")
	]);

let opaqueStyle: string = style([
    opacity(1.0),
    transform(`translate3d(px(0), px(0), px(0))),
    transition(~duration=100, ~delay=1, ~timingFunction=`ease, "opacity")
	]);

/* still in Greeting.re */
let component = ReasonReact.statelessComponent("Paper");

let make = (children) => {
	...component,
	render: _self =>
		<AnimatedMount mountedStyle=opaqueStyle
								   unmountedStyle=clearStyle
			>
			...children
		</AnimatedMount>
};