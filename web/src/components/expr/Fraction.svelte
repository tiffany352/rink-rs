<script lang="typescript">
  import Node from "./Node.svelte";
  import * as prec from "../../util/precedence";

  export let value: prec.Fraction;
</script>

<!-- prettier-ignore -->
{#if true
  }<sup
    ><Node
      value={value.left}
      precedence={prec.expectedPrecedence(value.op) - 1}
    /></sup
  ><span>&#x2215;</span
  ><sub
    ><Node
      value={value.right}
      precedence={prec.expectedPrecedence(value.op) - 1}
  /></sub
>{/if}
