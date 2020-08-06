<script lang="typescript">
  import { Precedence } from "../util/precedence";

  export let precedence: Precedence;
  export let expected: Precedence;
</script>

{#if precedence < expected}
  <span>(</span>
{/if}
<slot />
{#if precedence < expected}
  <span>)</span>
{/if}
