import "./styles.css";
import { init } from "./Diagram";

document.getElementById("app").innerHTML = `
<h1>Hello Vanilla!</h1>
<button onclick="refreshModel()">Generate random model</button>
<div class="Diagram" id="myDiagramDiv" />
`;

init();
