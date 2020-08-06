<script lang="typescript">
  import type { UnitListReply } from "../../util/reply";
  import Number from "../Number.svelte";

  export let value: UnitListReply;
</script>

{#each value.list as unit, i}
  {#if i != 0}
    <span>,</span>
  {/if}
  <Number number={unit} />
{/each}

<Number number={value.rest} />
