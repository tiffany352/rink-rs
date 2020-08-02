import { Query } from "~/../rink-js/Cargo.toml";

console.log("hello world");

let query = new Query("1 + 1");
console.log("query", query.getExpr());
