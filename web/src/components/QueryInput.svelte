<script lang="typescript">
  import { goto } from "@sapper/app";
  import { query } from "../stores";

  export let text: string = "";

  query.subscribe((value) => {
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
    border-radius: 5px;
    padding: 0.3em;
    flex-grow: 1;
  }

  svg {
    stroke: rgb(60, 60, 60);
  }

  .header {
    margin-top: 1em;
    display: flex;
    flex-direction: row;
    justify-content: stretch;
    align-items: center;
  }

  .home {
    padding: calc(0.3em - 1px);
    margin: 0.2em;
    background-color: rgb(255, 255, 255);
    border: 1px solid rgb(60, 60, 60);
    border-radius: 5px;
  }

  .home:hover,
  .home:focus {
    background-color: rgb(240, 240, 240);
    border: 1px solid rgb(20, 20, 20);
  }
</style>

<div class="header">
  <a href="/" class="home" title="Home" role="navigation" alt="Home">
    <svg
      role="figure"
      xmlns="http://www.w3.org/2000/svg"
      width="24"
      height="24"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      stroke-width="2"
      stroke-linecap="round"
      stroke-linejoin="round"
      class="feather feather-home">
      <title>Home</title>
      <path d="M3 9l9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z" />
      <polyline points="9 22 9 12 15 12 15 22" />
    </svg>
  </a>

  <form
    action="/query"
    method="get"
    on:submit={handleSubmit}
    on:click={handleFormClick}
    aria-label="Search">
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
      <title>Search</title>
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
</div>
