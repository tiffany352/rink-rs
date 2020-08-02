import "@webcomponents/webcomponentsjs/webcomponents-loader.js";
import "@webcomponents/webcomponentsjs/custom-elements-es5-adapter.js";
import App from "./App.svelte";

console.log("hello world");
const app = new App({
  target: document.body,
  props: {},
});

export default app;
