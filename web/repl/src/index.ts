import init, * as rink from 'rink-js';

init().then(() => {
  console.log('hello', rink);

  let rinkDiv: HTMLElement = document.querySelector("#rink-outputs")!;
  let form: HTMLFormElement = document.querySelector("#rink-query")!;
  let textEntry: HTMLInputElement = document.querySelector("#query")!;

  let ctx = new rink.Context();
  ctx.setSavePreviousResult(true);
  console.log(ctx);

  let welcome = document.createElement("p");
  welcome.innerText = `Rink ${rink.version()}`;
  rinkDiv.appendChild(welcome);

  form.addEventListener("submit", (event) => {
    event.preventDefault();

    let quote = document.createElement("blockquote");
    quote.innerText = textEntry.value;
    rinkDiv.appendChild(quote);

    let query = new rink.Query(textEntry.value);
    let tokens = ctx.eval_tokens(query);
    console.log(tokens);

    let p = document.createElement("p");
    let ul: HTMLUListElement | null = null;
    let cur: HTMLElement = p;
    for (const token of tokens) {
      if (token.fmt == "plain") {
        cur.appendChild(new Text(token.text));
      } else if (token.fmt == "pow") {
        let text = token.text.replace(/^\^/, '');
        let sup = document.createElement("sup");
        sup.innerText = text;
        cur.appendChild(sup);
      } else if (token.fmt == "list_begin") {
        ul = document.createElement("ul");
        p.appendChild(ul);
        let li = document.createElement("li");
        cur = li;
        ul.appendChild(li);
      } else if (token.fmt == "list_sep" && ul) {
        let li = document.createElement("li");
        cur = li;
        ul.appendChild(li);
      } else {
        let span = document.createElement("span");
        span.classList.add(`hl-${token.fmt.replace('_', '-')}`);
        span.innerText = token.text;
        cur.appendChild(span);
      }
    }

    rinkDiv.appendChild(p);
    textEntry.value = "";
  });
});
