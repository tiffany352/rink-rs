<script lang="typescript">
  import * as expr from "../../util/expr";
  import * as prec from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: expr.MulExpr;
  export let precedence: prec.Precedence;
</script>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={prec.Precedence.Mul}
  >{#each value.exprs as item, i
    }{#if i != 0
      }<span
        >&ThinSpace;</span
      >{/if
    }<Node value={item} precedence={prec.Precedence.Div}
  />{/each
}</PrecedenceWrapper>
