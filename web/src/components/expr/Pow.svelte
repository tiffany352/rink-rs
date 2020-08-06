<script lang="typescript">
  import * as prec from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: prec.PowExpr;
  export let precedence: prec.Precedence;
</script>

<style>
  .hidden {
    font-size: 0;
    color: transparent;
  }
</style>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={prec.Precedence.Pow}
  ><Node value={value.left} precedence={prec.Precedence.Term}
    /><span class="hidden">^</span
    ><sup
      ><Node value={value.right} precedence={prec.Precedence.Term}
    /></sup
  ></PrecedenceWrapper
>
