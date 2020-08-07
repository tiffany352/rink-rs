<script context="module">
  import Rink from "../../util/rink";
  import Result from "../../components/reply/Result.svelte";
  import Card from "../../components/Card.svelte";
  import Container from "../../components/Container.svelte";
  import OpenGraph from "../../components/OpenGraph.svelte";
  import { describe } from "../../util/reply";

  export async function preload(page, session) {
    let { name } = page.params;
    const rink = await Rink.getRink();
    const expr = rink.parse(name || "");
    const context = rink.createContext();
    context.setTime(new Date());
    const value = context.eval(expr);

    return { value, name };
  }
</script>

<script>
  export let value = null;
  export let name = "";
</script>

<svelte:head>
  <OpenGraph title="Rink - {name}" description={value ? describe(value) : ''} />
</svelte:head>

<Container>
  {#if value != null}
    <Card>
      <Result {value} />
    </Card>
  {/if}
</Container>
