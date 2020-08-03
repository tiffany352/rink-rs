<script lang="typescript">
  import * as reply from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: reply.ConversionReply;
</script>

<p>
  <NumberParts number={value.value} />
</p>
