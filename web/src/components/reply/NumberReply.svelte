<script lang="typescript">
  import type { NumberReply } from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: NumberReply;
</script>

<p>
  <NumberParts number={value} />
</p>
