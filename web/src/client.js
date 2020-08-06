import wasm from "../../rink-js/Cargo.toml";
import Rink from "./util/rink";
import * as sapper from "@sapper/app";

sapper.start({
  target: document.querySelector("#sapper"),
});

async function loadRink() {
  let rink = await wasm();
  Rink.setRink(rink);
}

loadRink();
