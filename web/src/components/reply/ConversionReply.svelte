<script lang="typescript">
  import type { ConversionReply } from "../../util/reply";
  import Number from "../Number.svelte";

  export let value: ConversionReply;
</script>

<p>
  <Number number={value.value} />
</p>
