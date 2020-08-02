import wasm from "../../rink-js/Cargo.toml";
import Rink from "./util/rink";
import sirv from "sirv";
import polka from "polka";
import compression from "compression";
import * as sapper from "@sapper/server";

const { PORT, NODE_ENV } = process.env;
const dev = NODE_ENV === "development";

polka() // You can also use Express
  .use(
    compression({ threshold: 0 }),
    sirv("static", { dev }),
    sapper.middleware()
  )
  .listen(PORT, (err) => {
    if (err) console.log("error", err);
  });

async function loadRink() {
  let rink = await wasm();
  Rink.setRink(rink);
}

loadRink();
