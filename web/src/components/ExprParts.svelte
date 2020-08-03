<script lang="typescript">
  import * as reply from "../util/reply";

  export let expr: reply.ExprParts[];
</script>

<style>
  .highlight {
    font-weight: 500;
  }
</style>

<span>
  {#each expr as part, i}
    {#if i != 0}
      <span>&ThinSpace;</span>
    {/if}
    {#if part.type == 'literal'}
      <span>{part.text}</span>
    {:else if part.type == 'unit'}
      <span class="highlight">{part.name}</span>
    {/if}
  {/each}
</span>
