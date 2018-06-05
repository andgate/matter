let component = ReasonReact.statelessComponent("ModuleItem");

let make = (~modl: ModuleData.module_, _children) =>
{
	...component,
	
	render: _self => {
		/* We need a recursive function to render the list */
		let rec renderModule = (m) => {
			let moduleChildren =
          Array.map(
            (child: ModuleData.module_) => <li>{renderModule(child)}</li>,
            m.children
          ); 

			<ul>
				<li>{ReasonReact.string(m.name)}</li>
				<ul>
					{ReasonReact.array(moduleChildren)}
				</ul>	
			</ul>;
		};

		renderModule(modl);
	}
};