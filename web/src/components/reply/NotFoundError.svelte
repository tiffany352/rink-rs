<script lang="typescript">
  import type { NotFoundError } from "../../util/reply";

  export let value: NotFoundError;
</script>

{#if value.suggestion != null}
  <!-- prettier-ignore -->
  <p>
    No such unit {value.got}, did you mean
    <a href={`/unit/${value.suggestion}`}>{value.suggestion}</a>?
  </p>
{:else}
  <p>No such unit {value.got}.</p>
{/if}
