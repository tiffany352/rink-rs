<script lang="typescript">
  import wasm from "../../../rink-js/Cargo.toml";
  import Result from "./Result.svelte";

  export let queryText: string = "";
  export let result: any = null;
  let rink: any = null;

  async function loadRink() {
    if (rink != null) {
      return rink;
    }
    rink = await wasm();
    return rink;
  }

  export async function handleChange(
    event: Event & { target: EventTarget & HTMLInputElement }
  ) {
    console.log("handleChange");
    const exports = await loadRink();
    let expr = new exports.Query(queryText);
    let context = new exports.Context();
    context.setTime(new Date());
    result = context.eval(expr);
    //result = expr.getExpr();
    console.log("result", result);
  }
</script>

<svelte:head>
  <title>Rink</title>
</svelte:head>

<p>hello from svelte</p>
<input bind:value={queryText} on:change={handleChange} />
<Result value={result} />
