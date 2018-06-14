let component = ReasonReact.statelessComponent("ExprView");

let make = (~exp: Syntax.exp, _children) => {
  ...component,
  
  render: (self) => {Syntax.render(exp)},
};