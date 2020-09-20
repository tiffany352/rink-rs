<script lang="typescript">
  import type { Duration, DurationReply, NumberParts } from "../../util/reply";
  import Dimensionality from "../Dimensionality.svelte";

  export let value: DurationReply;

  let keys: (keyof Duration)[] = [
    "years",
    "months",
    "weeks",
    "days",
    "hours",
    "minutes",
    "seconds",
  ];

  $: values = keys
    .map((key) => value[key])
    .filter((value) => value.exactValue != "0");
</script>

{#each values as value, i}
  {#if i != 0}<span>, </span>{/if}
  <span>{value.exactValue}</span>
  <Dimensionality quantity={value.rawUnit} />
{:else}
  <span>0</span>
  <Dimensionality quantity={{ seconds: 1 }} />
{/each}
