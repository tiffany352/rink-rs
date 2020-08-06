<script lang="typescript">
  import type { SearchReply } from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: SearchReply;
</script>

<ul>
  {#each value.results as result}
    <li>
      <NumberParts number={result} />
    </li>
  {/each}
</ul>
