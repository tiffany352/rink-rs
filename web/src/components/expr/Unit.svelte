<script lang="typescript">
  import type { UnitExpr } from "../../util/expr";

  export let value: UnitExpr;
</script>

<a href={`/unit/${value.name}`}>{value.name}</a>
