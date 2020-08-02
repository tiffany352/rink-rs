<script context="module">
  import wasm from "../../../rink-js/Cargo.toml";
  import Result from "../components/Result.svelte";

  export async function preload() {
    const rink = await wasm();
    const expr = new rink.Query("meter");
    const context = new rink.Context();
    context.setTime(new Date());
    const value = context.eval(expr);

    return { value };
  }
</script>

<script>
  export let value;
</script>

<svelte:head>
  <title>Rink</title>
</svelte:head>

<p>test ssr page</p>
<Result {value} />
