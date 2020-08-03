<script lang="typescript">
  import * as reply from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: reply.UnitsForReply;
</script>

<h2>
  Units for
  <NumberParts number={value.of} />
</h2>

{#each value.units as category}
  <h3>{category.category || 'Uncategorized'}</h3>
  <ul>
    {#each category.units as unit}
      <li>{unit}</li>
    {/each}
  </ul>
{/each}
