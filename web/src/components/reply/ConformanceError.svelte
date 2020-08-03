<script lang="typescript">
  import * as reply from "../../util/reply";

  export let value: reply.ConformanceError;
</script>

<style>
  .capitalize::first-letter {
    text-transform: capitalize;
  }
</style>

<h3>Conformance Error</h3>
<p>
  {value.left.quantity} ({value.left.dimensions}) != {value.right.quantity} ({value.right.dimensions})
</p>
<h4>Suggestions</h4>
<ul>
  {#each value.suggestions as suggestion}
    <li class="capitalize">{suggestion}</li>
  {/each}
</ul>
