<script context="module">
  import Rink from "../../util/rink";
  import Result from "../../components/reply/Result.svelte";
  import Card from "../../components/Card.svelte";
  import Container from "../../components/Container.svelte";
  import { query } from "../../stores";

  export async function preload(page, session) {
    let { input } = page.params;
    query.set(input);
    const rink = await Rink.getRink();
    const expr = rink.parse(input || "");
    const context = rink.createContext();
    context.setTime(new Date());
    const value = context.eval(expr);

    return { value, input };
  }
</script>

<script>
  import { onMount } from "svelte";
  import { stores } from "@sapper/app";

  export let value = null;
  export let input = null;

  onMount(() => {
    query.set(input);
  });
</script>

<svelte:head>
  <title>Rink - query</title>
</svelte:head>

<Container>
  {#if value != null}
    <Card>
      <Result {value} />
    </Card>
  {/if}
</Container>
