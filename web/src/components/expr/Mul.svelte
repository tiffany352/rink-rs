<script lang="typescript">
  import type { MulExpr } from "../../util/expr";
  import { Precedence } from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: MulExpr;
  export let precedence: Precedence;
</script>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={Precedence.Mul}
  >{#each value.exprs as item, i
    }{#if i != 0
      }<span
        >&ThinSpace;</span
      >{/if
    }<Node value={item} precedence={Precedence.Mul}
  />{/each
}</PrecedenceWrapper>
