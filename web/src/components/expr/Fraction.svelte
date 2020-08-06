<script lang="typescript">
  import type { Fraction } from "../../util/precedence";
  import { Precedence } from "../../util/precedence";
  import Node from "./Node.svelte";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";

  export let value: Fraction;
  export let precedence: Precedence;
</script>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={Precedence.Mul}
><sup
    ><Node
      value={value.left}
      precedence={Precedence.Mul}
    /></sup
  ><span>&#x2215;</span
  ><sub
    ><Node
      value={value.right}
      precedence={Precedence.Mul}
  /></sub
></PrecedenceWrapper>
