<script lang="typescript">
  import NavButton from "../components/NavButton.svelte";
  import Container from "../components/Container.svelte";
</script>

<style>
  h1 {
    text-align: center;
    padding: 0.5em;
    margin: 0;
  }

  nav {
    display: flex;
    flex-direction: row;
    justify-content: center;
    width: 100%;
    border-bottom: 2px solid rgb(200, 200, 200);
  }
</style>

<svelte:head>
  <title>Rink</title>
</svelte:head>

<Container header="normal">
  <h1 slot="header">Rink</h1>

  <nav role="navigation" aria-label="Links">
    <NavButton
      text="GitHub"
      icon="/svg/GitHub.svg"
      href="https://github.com/tiffany352/rink-rs/" />
    <NavButton
      text="Manual"
      icon="/svg/book-open.svg"
      href="https://github.com/tiffany352/rink-rs/wiki/Rink-Manual" />
  </nav>

  <p>
    Rink is a unit-aware calculator. It can be used for physics and engineering
    calculations, as well as dimensionality analysis.
  </p>

  <p>
    Rink supports most systems of measurements including SI, CGS, natural,
    international customary, US customary, UK customary, as well as historical
    measurements. In addition, Rink supports currency conversions.
  </p>

  <p>
    This is the web interface to Rink, which is implemented as a progressive web
    app.
  </p>
</Container>
