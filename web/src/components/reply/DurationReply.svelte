<script lang="typescript">
  import * as reply from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: reply.DurationReply;

  let keys: (keyof reply.DurationReply)[] = [
    "years",
    "months",
    "weeks",
    "days",
    "hours",
    "minutes",
    "seconds",
  ];

  function describe(value: reply.DurationReply): string {
    let out = [];

    for (const key of keys) {
      console.log(key);
      const entry = (value[key] as any) as reply.NumberParts;
      if (entry.exactValue != "0") {
        out.push(entry.exactValue + " " + key);
      }
    }

    return out.join(", ");
  }

  $: description = describe(value);
</script>

{description}
