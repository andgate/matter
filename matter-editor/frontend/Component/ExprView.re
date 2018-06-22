open Css;

let style = style([
  display(flexBox),
  flex(1),
  flexGrow(1),
  flexBasis(auto)
]);

let component = ReasonReact.statelessComponent("ExprView");

let make = (~root: Syntax.inode, ~cursor: int, ~onKeyDown, _children) => {
  ...component,
  
  render: (self) => 
    <div tabIndex=0 onKeyDown=onKeyDown className=style>
      {Syntax.render(root)}
    </div>,
};