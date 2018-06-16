let component = ReasonReact.statelessComponent("ExprView");

let make = (~zexp: Syntax.zexp, ~onKeyDown, _children) => {
  ...component,
  
  render: (self) => 
    <div tabIndex=0 autoFocus=true onKeyDown={onKeyDown}>
      {Syntax.render(zexp)}
    </div>,
};