<script lang="typescript">
  import * as reply from "../../util/reply";

  export let value: reply.FactorizeReply;
</script>

<style>
  li > div {
    display: inline-flex;
    flex-direction: row;
  }

  .space {
    width: 0.25em;
  }
</style>

<ul>
  {#each value.factorizations as unit}
    <li>
      <div>
        {#each Object.entries(unit) as [dim, pow], i}
          {#if i != 0}
            <span class="space">&nbsp;</span>
          {/if}
          <span>{dim}</span>
          <sup>{pow}</sup>
        {/each}
      </div>
    </li>
  {/each}
</ul>
