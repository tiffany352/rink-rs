<script lang="typescript">
  import QueryInput from "./QueryInput.svelte";

  export let header: "dark" | "normal" = "normal";
</script>

<style>
  .container {
    max-width: 60em;
    margin: 0 auto;
  }

  .header {
    border-top-left-radius: 5px;
    border-top-right-radius: 5px;
    border: 1px solid rgb(200, 200, 200);
    background-color: white;
    border-bottom: none;
    margin-top: 1em;
    min-height: 5px;
  }

  .dark {
    background-color: rgb(30, 30, 30);
    color: white;
  }

  .content {
    border: 1px solid rgb(200, 200, 200);
    border-bottom-left-radius: 5px;
    border-bottom-right-radius: 5px;
    padding: 1em;
    padding-top: 0;
    background-color: white;
    border-top: none;
  }
</style>

<div class="container">
  <QueryInput />

  <div class={`header ${header}`}>
    <slot name="header" />
  </div>
  <div class="content" role="main">
    <slot />
  </div>
</div>
