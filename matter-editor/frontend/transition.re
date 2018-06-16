[@bs.module "react-transition-group/Transition"] external transition: ReasonReact.reactClass = "Transition";

let make = (~style, isIn: bool, timeout: int, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=transition,
    ~props={
			"style": style
		, "in": isIn
		, "timeout": timeout
		},
    children,
  );