import { LitElement, html } from "lit-element";

class App extends LitElement {
  render() {
    return html`
      <div>
        Hello from LitElement
      </div>
    `;
  }
}

customElements.define("rink-app", App);
