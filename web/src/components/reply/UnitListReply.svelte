<script lang="typescript">
  import type { UnitListReply } from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: UnitListReply;
</script>

{#each value.list as unit, i}
  {#if i != 0}
    <span>,</span>
  {/if}
  <NumberParts number={unit} />
{/each}

<NumberParts number={value.rest} />
