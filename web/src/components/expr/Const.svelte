<script lang="typescript">
  import * as expr from "../../util/expr";
  import Numeric from "../Numeric.svelte";

  export let value: expr.ConstExpr;
</script>

<span>
  <Numeric number={value.value} />
</span>
