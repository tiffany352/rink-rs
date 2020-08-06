<script context="module">
  export async function preload(page, session) {
    let { input } = page.query;
    const slug = encodeURIComponent(input || "");
    return this.redirect(302, `/query/${slug}`);
  }
</script>
