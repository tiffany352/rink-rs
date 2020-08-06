<script lang="typescript">
  import type { UnitsForReply } from "../../util/reply";
  import Dimensionality from "../Dimensionality.svelte";

  export let value: UnitsForReply;
  console.log(value);
</script>

<!-- prettier-ignore -->
<h2>
  Units for {value.of.quantity}
  (<Dimensionality quantity={value.of.rawDimensions} />)
</h2>

{#each value.units as category}
  <h3>{category.category || 'Uncategorized'}</h3>
  <ul>
    {#each category.units as unit}
      <li>
        <a href={`/unit/${unit}`}>{unit}</a>
      </li>
    {/each}
  </ul>
{/each}
