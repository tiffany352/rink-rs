<script lang="typescript">
  import type { SearchReply } from "../../util/reply";
  import Number from "../Number.svelte";

  export let value: SearchReply;
</script>

<ul>
  {#each value.results as result}
    <li>
      <Number number={result} />
    </li>
  {/each}
</ul>
