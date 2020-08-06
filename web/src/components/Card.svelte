<script>
  import { children } from "svelte/internal";
</script>

<style>
  .card-inner {
    border-left: 4px solid #ddd;
    padding: 0.1em 0.5em;
    background-color: rgb(250, 250, 250);
  }

  .card {
    padding: 0.5em;
  }
</style>

<div class="card">
  <div class="card-inner">
    <slot />
  </div>
</div>
