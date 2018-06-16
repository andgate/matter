[@bs.module "react-transition-group/CSSTransition"] external csstransition: ReasonReact.reactClass = "CSSTransition";

let make = (~style, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=csstransition,
    ~props={
			"style": style
		},
    children,
  );