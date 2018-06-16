[@bs.module "react-transition-group/TransitionGroup"] external transitiongroup: ReasonReact.reactClass = "TransitionGroup";

let make = (~style, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=transitiongroup,
    ~props={"style": style},
    children,
  );