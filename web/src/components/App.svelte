<script lang="typescript">
  import wasm from "../../../rink-js/Cargo.toml";

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
    result = new exports.Query(queryText);
  }
</script>

<svelte:head>
  <title>Rink</title>
</svelte:head>

<p>hello from svelte</p>
<input bind:value={queryText} on:change={handleChange} />
<pre>{JSON.stringify(result && result.getExpr(), null, 2)}</pre>
