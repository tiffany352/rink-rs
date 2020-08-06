<script lang="typescript">
  import type { DateReply } from "../../util/reply";

  export let value: DateReply;

  const format = new Intl.DateTimeFormat(undefined, {
    year: "numeric",
    month: "long",
    day: "numeric",
    weekday: "long",
    hour: "numeric",
    minute: "numeric",
    second: "numeric",
    timeZoneName: "short",
  });
</script>

<style>
  .small {
    color: rgb(120, 120, 120);
    font-size: 0.8em;
  }
</style>

<p>{format.format(new Date(value.rfc3339))}</p>
<p class="small">{new Date(value.rfc3339).toUTCString()}</p>
