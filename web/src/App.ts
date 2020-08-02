import { LitElement, html, property, customElement } from "lit-element";
import { Query } from "~/../rink-js/Cargo.toml";

@customElement("rink-app")
export class App extends LitElement {
  @property({ type: String })
  text: string = "";
  @property({ type: Query })
  query: Query | null = null;

  render() {
    return html`
      <div>
        Hello from LitElement
        <div>
          <input
            name="query"
            type="text"
            @change="${this.handleChange}"
            @input="${this.handleSubmit}"
          />
          <pre>${JSON.stringify(this.query?.getExpr(), null, 2)}</pre>
        </div>
      </div>
    `;
  }

  handleChange = (event) => {
    console.log("handleChange");
    this.text = event.target.value;
    this.query = new Query(this.text);
  };
  handleSubmit = (event) => {
    console.log("handleSubmit");
  };
}
