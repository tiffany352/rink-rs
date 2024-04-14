import init, * as rink from 'rink-js';

init().then(() => {
  console.log('hello', rink);

  let rinkDiv: HTMLElement = document.querySelector("#rink")!;
  let form: HTMLFormElement = document.querySelector("#rink-query")!;
  let textEntry: HTMLInputElement = document.querySelector("#query")!;

  let ctx = new rink.Context();
  console.log(ctx);

  form.addEventListener("submit", (event) => {
    event.preventDefault();
    let query = new rink.Query(textEntry.value);
    let tokens = ctx.eval_tokens(query);
    console.log(tokens);

    let p = document.createElement("p");
    for (const token of tokens) {
      let span = document.createElement("span");
      span.classList.add(`hl-${token.fmt}`);
      span.innerText = token.text;
      p.appendChild(span);
    }

    rinkDiv.appendChild(p);
  });
});
