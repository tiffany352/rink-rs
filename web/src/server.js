import wasm from "../../rink-js/Cargo.toml";
import Rink from "./util/rink";
import sirv from "sirv";
import express from "express";
import compression from "compression";
import * as sapper from "@sapper/server";
import { updateCurrency, currencyPath } from "./currency.ts";
import { existsSync } from "fs";

const { PORT, NODE_ENV } = process.env;
const dev = NODE_ENV === "development";

async function batchJob() {
  await updateCurrency();
  process.exit(0);
}

async function main() {
  if (!existsSync(currencyPath)) {
    await updateCurrency();
  }

  express()
    .use("data", sirv(".", { dev }))
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
}

if (process.argv[2] == "batch-job") {
  batchJob();
} else {
  main();
}
