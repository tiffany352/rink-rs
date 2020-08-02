<script lang="typescript">
  import { Query } from "~/../rink-js/Cargo.toml";

  export let queryText: string = "";
  export let result: Query | null = null;

  export function handleChange(
    event: Event & { target: EventTarget & HTMLInputElement }
  ) {
    console.log("handleChange");
    result = new Query(queryText);
  }
</script>

<svelte:head>
  <title>Rink</title>
</svelte:head>

<p>hello from svelte</p>
<input bind:value={queryText} on:change={handleChange} />
<pre>{JSON.stringify(result?.getExpr(), null, 2)}</pre>
