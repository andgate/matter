open Css;


/* State declaration */
type state = {
  style: string
};


type action =
  | Mount
  | Unmount;


/* still in Greeting.re */
let component = ReasonReact.reducerComponent("AnimatedMount");

let make = (~mountedStyle: string, ~unmountedStyle: string, _children) => {
	...component,

	initialState: () => {
		style: unmountedStyle
	},


  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Mount => ReasonReact.Update({style: mountedStyle})
    | Unmount => ReasonReact.Update({style: unmountedStyle})
    },

	render: self =>
		ReasonReact.createDomElement("div", ~props={"className": self.state.style}, _children)
};