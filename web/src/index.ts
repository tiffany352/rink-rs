import "@webcomponents/webcomponentsjs/webcomponents-loader.js";
import "@webcomponents/webcomponentsjs/custom-elements-es5-adapter.js";
import { Query } from "~/../rink-js/Cargo.toml";
import "./App";

console.log("hello world");

let query = new Query("1 + 1");
console.log("query", query.getExpr());
