<script lang="typescript">
  import type { ConversionReply } from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: ConversionReply;
</script>

<p>
  <NumberParts number={value.value} />
</p>
