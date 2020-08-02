<script lang="typescript">
  export let value: unknown;
</script>

<pre>{typeof value == 'string' ? value : JSON.stringify(value, null, 2)}</pre>
