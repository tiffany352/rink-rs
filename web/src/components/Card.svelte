<script>
  import { children } from "svelte/internal";
</script>

<style>
  .card {
    border-left: 4px solid #ddd;
    padding: 0.5em;
    background-color: white;
  }
</style>

<div class="card">
  <slot />
</div>
