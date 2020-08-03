<script lang="typescript">
  import * as reply from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: reply.SearchReply;
</script>

<ul>
  {#each value.results as result}
    <li>
      <NumberParts number={result} />
    </li>
  {/each}
</ul>
