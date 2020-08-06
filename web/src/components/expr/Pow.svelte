<script lang="typescript">
  import { Precedence } from "../../util/precedence";
  import type { PowExpr } from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: PowExpr;
  export let precedence: Precedence;
</script>

<style>
  .hidden {
    font-size: 0;
    color: transparent;
  }
</style>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={Precedence.Pow}
  ><Node value={value.left} precedence={Precedence.Term}
    /><span class="hidden">^</span
    ><sup
      ><Node value={value.right} precedence={Precedence.Term}
    /></sup
  ></PrecedenceWrapper
>
