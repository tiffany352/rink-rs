<script lang="typescript">
  import type { Dimensionality } from "../util/reply";

  export let quantity: Dimensionality;

  type DimensionalityArray = [string, number][];

  function readUnits(
    quantity: Dimensionality
  ): { numer: DimensionalityArray; denom: DimensionalityArray } {
    const numer: DimensionalityArray = [];
    const denom: DimensionalityArray = [];
    if (quantity) {
      for (const [dim, pow] of Object.entries(quantity)) {
        if (pow > 0) {
          numer.push([dim, pow]);
        } else {
          denom.push([dim, pow]);
        }
      }
    }
    numer.sort();
    denom.sort();
    return { numer, denom };
  }

  $: unit = readUnits(quantity);
</script>

<style>
  .unit > a {
    display: contents;
  }

  .hidden {
    font-size: 0;
    color: transparent;
  }
</style>

<!-- prettier-ignore -->
<span class="unit"
  >{#each unit.numer as [dim, pow], i
  }{#if i != 0
    }&ThinSpace;{/if
  }<a href={`/unit/${dim}`}
    ><span
    >{dim}</span
    >{#if pow != 1
    }<span class="hidden"
      >^</span
    ><sup
      >{pow}</sup
    >{/if
  }</a
  >{/each
  }{#if unit.denom.length > 0
  }<span
    >&MediumSpace;/&MediumSpace;</span
  >{#each unit.denom as [dim, pow]
    }<a href={`/unit/${dim}`}
    ><span
      >{dim}</span
    >{#if pow != -1
      }<span class="hidden"
      >^</span
      ><sup
      >{-pow}</sup
    >{/if
    }</a
  >{/each
  }{/if
}</span>
