<script lang="typescript">
  export let text: string;
  export let icon: string;
  export let href: string;
</script>

<style>
  a {
    padding: 0.25em;
    padding: 0.5em;
    display: flex;
    flex-direction: row;
    align-items: center;
    gap: 0.5em;
    color: rgb(60, 60, 60);
    text-decoration: unset;
    border-radius: 2px;
  }

  a:hover {
    color: rgb(10, 10, 10);
    border-bottom: 3px solid rgb(120, 120, 120);
    padding-bottom: calc(0.5em - 3px);
  }
</style>

<a {href}>
  <img src={icon} alt="" width="24" height="24" />
  {text}
</a>
