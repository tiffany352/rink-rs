<script lang="typescript">
  import type { ConstExpr } from "../../util/expr";
  import Numeric from "../Numeric.svelte";

  export let value: ConstExpr;
</script>

<span>
  <Numeric number={value.value} />
</span>
