<script lang="typescript">
  import { stores } from "@sapper/app";
  export let title: string;
  export let type: string = "website";
  export let image: string = "/images/icons/icon-512x512.png";
  export let description: string = "";

  const { page } = stores();

  function getUrl(page: any) {
    const url = page.host + page.path;

    return `https://${url}`;
  }
</script>

<title>{title}</title>
<meta property="og:title" content={title} />
<meta property="og:type" content={type} />
<meta property="og:url" content={getUrl($page)} />
<meta property="og:image" content="https://{$page.host}{image}" />
<meta property="og:description" content={description} />
<meta property="og:site_name" content="Rink" />
