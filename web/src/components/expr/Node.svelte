<script lang="typescript">
  import type { Expr } from "../../util/expr";
  import { Precedence, isPow, isFraction } from "../../util/precedence";
  import Unit from "./Unit.svelte";
  import Quote from "./Quote.svelte";
  import Const from "./Const.svelte";
  import Date from "./Date.svelte";
  import Pow from "./Pow.svelte";
  import Fraction from "./Fraction.svelte";
  import Binop from "./Binop.svelte";
  import Unaryop from "./Unaryop.svelte";
  import Mul from "./Mul.svelte";
  import Of from "./Of.svelte";
  import Call from "./Call.svelte";

  export let value: Expr;
  export let precedence: Precedence = Precedence.Equals;
</script>

<!-- prettier-ignore -->
{#if value.type == 'unit'
  }<Unit {value} 
/>{:else if value.type == 'quote'
  }<Quote {value}
/>{:else if value.type == 'const'
  }<Const {value}
/>{:else if value.type == 'date'
  }<Date {value}
/>{:else if isPow(value)
  }<Pow {value} {precedence}
/>{:else if isFraction(value)
  }<Fraction {value} {precedence}
/>{:else if value.type == 'binop'
  }<Binop {value} {precedence}
/>{:else if value.type == 'unaryop'
  }<Unaryop {value}
/>{:else if value.type == 'mul'
  }<Mul {value} {precedence}
/>{:else if value.type == 'of'
  }<Of {value} {precedence}
/>{:else if value.type == 'call'
  }<Call {value}
/>{:else if value.type == 'error'
  }<span style="color: red"
    >{value.message}</span
>{/if}
