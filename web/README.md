# Rink Web

This is a web interface to Rink that works by compiling Rink to
WebAssembly. The frontend is written in TypeScript using Svelte +
Sapper.

This is a progressive web app, and also supports server side rendering.

## Development

Make sure you have Rustup and the latest Stable Rust toolchain.

All the dependencies need to be installed by running:

```
npm install
```

Once that's done, you can start the dev server by running:

```
npm run dev
```

This will automatically compile Rink to wasm for you. It will detect
changes made to rink-js and recompile, but changes to rink-core will
require restarting the dev server.

## Deployment

To deploy rink-web, there's a couple of steps you'll need to do:

1. Run `npx sapper build build`. This will compile the app and put it
   into `build/`.
2. Move your `node_modules` out of the way and run `npm install --production` to get just the production dependencies.
3. Copy `build/`, `node_modules/` and `static/` to your web server.
4. Run `node build/` to start the server. It will respect the `PORT` and
   `NODE_ENV` environment variables, which default to `3000` and
   `production` respectively.

The following directories can be served statically, as long as there's
still a fallback that reverse proxies the node server:

- `static/` is mounted on `/`.
- `build/client/` is mounted on `/client/`.
