<script lang="typescript">
  import * as expr from "../../util/expr";

  export let value: expr.UnitExpr;
</script>

<a href={`/unit/${value.name}`}>{value.name}</a>
