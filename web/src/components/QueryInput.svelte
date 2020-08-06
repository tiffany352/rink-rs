<script lang="typescript">
  import { goto } from "@sapper/app";
  import { query } from "../stores";

  export let text: string = "";

  query.subscribe((value) => {
    console.log("query updated", value);
    text = value;
  });

  let inputElement: HTMLInputElement;

  async function handleSubmit(event: Event & { target: HTMLFormElement }) {
    event.preventDefault();
    const slug = encodeURIComponent(text);
    await goto(`/query/${slug}`);
  }

  function handleFormClick() {
    inputElement.focus();
  }
</script>

<style>
  .input {
    font-size: 1.25em;
    flex-grow: 1;
    background: none;
    border: none;
    margin-left: 0.3em;
  }

  .input:focus {
    outline: none;
  }

  form {
    display: flex;
    background-color: rgb(255, 255, 255);
    border: 1px solid rgb(60, 60, 60);
    border-radius: 4px;
    padding: 0.3em;
    margin-top: 1em;
  }

  svg {
    stroke: rgb(60, 60, 60);
  }
</style>

<form
  action="/query"
  method="get"
  on:submit={handleSubmit}
  on:click={handleFormClick}>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    stroke="currentColor"
    stroke-width="2"
    stroke-linecap="round"
    stroke-linejoin="round"
    class="feather feather-search">
    <circle cx="11" cy="11" r="8" />
    <line x1="21" y1="21" x2="16.65" y2="16.65" />
  </svg>

  <input
    bind:this={inputElement}
    class="input"
    type="search"
    name="input"
    bind:value={text}
    placeholder="Enter a query..." />
</form>
