<script lang="typescript">
  import { goto } from "@sapper/app";

  export let text: string = "";

  async function handleSubmit(event: Event & { target: HTMLFormElement }) {
    event.preventDefault();
    const slug = encodeURIComponent(text);
    await goto(`/query/${slug}`);
  }
</script>

<style>
  input {
    margin: 0.5em;
    font-size: 1.25em;
    width: 100%;
  }
</style>

<form action="/query" method="get" on:submit={handleSubmit}>
  <input name="input" bind:value={text} placeholder="Enter a query..." />
  <input type="submit" />
</form>
