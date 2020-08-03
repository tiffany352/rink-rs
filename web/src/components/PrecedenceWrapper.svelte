<script lang="typescript">
  import * as expr from "../util/expr";

  export let precedence: expr.Precedence;
  export let expected: expr.Precedence;
</script>

{#if precedence < expected}
  <span>(</span>
{/if}
<slot />
{#if precedence < expected}
  <span>)</span>
{/if}
