<script context="module">
  import Rink from "../util/rink";
  import Result from "../components/reply/Result.svelte";

  export async function preload() {
    const rink = await Rink.getRink();
    const expr = rink.parse("meter");
    const context = rink.createContext();
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
