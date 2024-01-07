import { Def } from "util/defs";
import fetch from "node-fetch";
import { parse } from "elementtree";
import { writeFile } from "atomically";
import { mkdir } from "fs/promises";
import ecbDefaults from "./ecb-defaults.json";

async function btc() {
  interface Format {
    timestamp: number;
    market_price_usd: number;
    hash_rate: number;
    totalbc: number;
  }

  const response = await fetch("https://api.blockchain.info/stats");
  console.log("Fetched", response.url, response.status, response.statusText);
  if (response.status != 200) {
    throw new Error(
      `Received ${response.status} ${response.statusText} from blockchain.info`
    );
  }
  const json: Format = await response.json();

  const date = new Date(json.timestamp);

  const defs: Def[] = [
    {
      name: "BTC",
      doc: null,
      category: "currencies",
      type: "unit",
      expr: `price of bitcoin`,
    },
    {
      name: "bitcoin",
      doc: `Properties of the global Bitcoin network. Sourced from <https://blockchain.info>. Current as of ${date.toUTCString()}`,
      category: "currencies",
      type: "substance",
      symbol: null,
      properties: [
        {
          name: "price",
          doc: "Current market price of 1 BTC.",
          category: "currencies",
          inputName: "bitcoin",
          input: "1",
          outputName: "bitcoin",
          output: `${json.market_price_usd} USD`,
        },
        {
          name: "hashrate",
          doc: "Current hash rate of the global network",
          category: null,
          inputName: "hashrate",
          input: "1",
          outputName: "rate",
          output: `${json.hash_rate} 1e9 'hash'/sec`,
        },
        {
          name: "total",
          doc: "Total number of BTC in circulation.",
          category: null,
          inputName: "total",
          input: "1",
          outputName: "bitcoin",
          output: `${json.totalbc} / 1e8`,
        },
      ],
    },
  ];

  return defs;
}

async function ecb() {
  const response = await fetch(
    "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"
  );
  console.log("Fetched", response.url, response.status, response.statusText);
  if (response.status != 200) {
    throw new Error(
      `Received ${response.status} ${response.statusText} from ecb.europea.eu`
    );
  }
  const body = await response.text();
  const doc = parse(body);
  const timestamp = doc.find(".//Cube[@time]")?.attrib.time || "unknown";

  const desc = `Sourced from European Central Bank. Current as of ${timestamp}.`;

  const defs: Def[] = [];
  const seen: Set<string> = new Set();
  for (const element of doc.findall(".//Cube")) {
    const currency = element.attrib.currency;
    const rate = element.attrib.rate;
    if (currency && rate) {
      seen.add(currency);
      defs.push({
        name: currency,
        doc: desc,
        category: "currencies",
        type: "unit",
        expr: `(1 / ${rate}) EUR`,
      });
    }
  }

  defs.push({
    name: "HRK",
    doc: "Croatian Kuna. Pinned to Euro at a fixed rate since 2022-01-01.",
    category: "currencies",
    type: "unit",
    expr: "(1 / 7.5345) EUR",
  });

  for (const [name, currency] of Object.entries(ecbDefaults)) {
    if (!seen.has(name)) {
      defs.push({
        name,
        doc: `Fetching live data failed. Fallback value provided from ${currency.source} on ${currency.time}.`,
        category: "currencies",
        type: "unit",
        expr: `(1 / ${currency.value}) EUR`,
      });
    }
  }
  return defs;
}

const DATA_DIR = process.env.DATA_DIR || "data";
export const currencyPath = `${DATA_DIR}/currency.json`;

export async function updateCurrency() {
  const [btcDefs, ecbDefs] = await Promise.all([btc(), ecb()]);
  const defs: Def[] = [...btcDefs, ...ecbDefs];

  await mkdir(DATA_DIR, { recursive: true });

  await writeFile(currencyPath, JSON.stringify(defs, undefined, "\t"), {
    encoding: "utf8",
  });
  console.log("Successfully updated file.");
}
