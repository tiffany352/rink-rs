<script lang="typescript">
  import type { NotFoundError } from "../../util/reply";

  export let value: NotFoundError;
</script>

{#if value.suggestion != null}
  <p>No such unit {value.got}, did you mean {value.suggestion}?</p>
{:else}
  <p>No such unit {value.got}.</p>
{/if}
