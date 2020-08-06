<script lang="typescript">
  import type { ConformanceError } from "../../util/reply";
  import Dimensionality from "../Dimensionality.svelte";

  export let value: ConformanceError;
</script>

<style>
  .capitalize::first-letter {
    text-transform: capitalize;
  }
</style>

<h3>Conformance Error</h3>

<!-- prettier-ignore -->
<p>
  <a href="/quantity/{value.left.quantity}">{value.left.quantity}</a>
  (<Dimensionality quantity={value.left.rawDimensions} />)
  !=
  <a href="/quantity/{value.right.quantity}">{value.right.quantity}</a>
  (<Dimensionality quantity={value.right.rawDimensions} />)
</p>
<h4>Suggestions</h4>
<ul>
  {#each value.suggestions as suggestion}
    <li class="capitalize">{suggestion}</li>
  {/each}
</ul>
