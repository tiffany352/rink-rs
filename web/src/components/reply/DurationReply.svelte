<script lang="typescript">
  import type { DurationReply, NumberParts } from "../../util/reply";

  export let value: DurationReply;

  let keys: (keyof DurationReply)[] = [
    "years",
    "months",
    "weeks",
    "days",
    "hours",
    "minutes",
    "seconds",
  ];

  function describe(value: DurationReply): string {
    let out = [];

    for (const key of keys) {
      const entry = (value[key] as any) as NumberParts;
      if (entry.exactValue != "0") {
        out.push(entry.exactValue + " " + key);
      }
    }

    return out.join(", ");
  }

  $: description = describe(value);
</script>

{description}
