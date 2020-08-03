<script lang="typescript">
  import * as reply from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: reply.NumberReply;
</script>

<p>
  <NumberParts number={value} />
</p>
