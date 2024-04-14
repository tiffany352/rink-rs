import init, * as rink from 'rink-js';

init().then(() => {
  console.log('hello', rink);

  let rinkDiv: HTMLElement = document.querySelector("#rink-outputs")!;
  let form: HTMLFormElement = document.querySelector("#rink-query")!;
  let textEntry: HTMLInputElement = document.querySelector("#query")!;

  let ctx = new rink.Context();
  console.log(ctx);

  form.addEventListener("submit", (event) => {
    event.preventDefault();

    let quote = document.createElement("blockquote");
    quote.innerText = textEntry.value;
    rinkDiv.appendChild(quote);

    let query = new rink.Query(textEntry.value);
    let tokens = ctx.eval_tokens(query);
    console.log(tokens);

    let p = document.createElement("p");
    for (const token of tokens) {
      if (token.fmt == "plain") {
        p.appendChild(new Text(token.text));
      } else if (token.fmt == "pow") {
        let text = token.text.replace(/^\^/, '');
        let sup = document.createElement("sup");
        sup.innerText = text;
        p.appendChild(sup);
      } else {
        let span = document.createElement("span");
        span.classList.add(`hl-${token.fmt}`);
        span.innerText = token.text;
        p.appendChild(span);
      }
    }

    rinkDiv.appendChild(p);
  });
});
