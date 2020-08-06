// See https://github.com/kaisermann/svelte-preprocess#with-svelte-vs-code
const sveltePreprocess = require("svelte-preprocess");

module.exports = {
  preprocess: sveltePreprocess({
    // ...svelte-preprocess options (optional)
  }),
  // ...other svelte options (optional)
};
