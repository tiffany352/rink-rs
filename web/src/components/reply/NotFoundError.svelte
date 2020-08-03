<script lang="typescript">
  import * as reply from "../../util/reply";

  export let value: reply.NotFoundError;
</script>

{#if value.suggestion != null}
  <p>No such unit {value.got}, did you mean {value.suggestion}?</p>
{:else}
  <p>No such unit {value.got}.</p>
{/if}
