type module_ = {
	name: string,
	children: array(module_)
};

let rec parseModuleJson = (json: Js.Json.t): module_ =>
	Json.Decode.{
		name: field("moduleName", string, json),
		children: field("moduleChildren", array(parseModuleJson), json)
	};


let modulesUrl = "http://127.0.0.1:8080/modules";

let fetchModules = () =>
  Js.Promise.(
    Axios.get(modulesUrl)
      |> then_(response => {
          Js.log(response##data);
          resolve(response);
        })
      |> then_(response => resolve(Array.map(parseModuleJson, response##data)))
  );