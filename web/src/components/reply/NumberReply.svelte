<script lang="typescript">
  import type { NumberReply } from "../../util/reply";
  import Number from "../Number.svelte";

  export let value: NumberReply;
</script>

<p>
  <Number number={value} />
</p>
