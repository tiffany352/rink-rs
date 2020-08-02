<script lang="typescript">
  import Rink from "../util/rink";
  import Result from "./Result.svelte";

  export let queryText: string = "";
  export let result: any = null;

  export async function handleChange(
    event: Event & { target: EventTarget & HTMLInputElement }
  ) {
    console.log("handleChange");
    const rink = await Rink.getRink();
    let expr = rink.parse(queryText);
    let context = rink.createContext();
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
