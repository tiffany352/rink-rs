<script lang="typescript">
  import type { NumberParts } from "../util/reply";

  export let number: NumberParts;

  type QuantityArray = [string, number][];

  function readUnits(
    number: NumberParts
  ): { numer: QuantityArray; denom: QuantityArray } {
    const numer: QuantityArray = [];
    const denom: QuantityArray = [];
    if (number.rawUnit) {
      for (const [dim, pow] of Object.entries(number.rawUnit)) {
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

  $: unit = readUnits(number);
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

{#if number.exactValue}
  {number.exactValue}
{:else if number.approxValue}approx. {number.approxValue}{/if}
{#if number.rawUnit}
  <!-- prettier-ignore -->
  <span class="unit"
    >{#each unit.numer as [dim, pow]
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
{/if}
<!-- prettier-ignore -->
{#if number.quantity}
    (<a href={`/quantity/${number.quantity}`}>{number.quantity}</a>)
{/if}
