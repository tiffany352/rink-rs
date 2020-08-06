<script context="module">
  import Rink from "../../util/rink";
  import Result from "../../components/reply/Result.svelte";
  import Card from "../../components/Card.svelte";
  import QueryInput from "../../components/QueryInput.svelte";

  export async function preload(page, session) {
    let { input } = page.query;
    const rink = await Rink.getRink();
    const expr = rink.parse(input || "");
    const context = rink.createContext();
    context.setTime(new Date());
    const value = context.eval(expr);

    return { value };
  }
</script>

<script>
  export let value = null;
</script>

<svelte:head>
  <title>Rink - query</title>
</svelte:head>

<QueryInput />
{#if value != null}
  <Card>
    <Result {value} />
  </Card>
{/if}
