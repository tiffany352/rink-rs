import fetch from "node-fetch";

let cache: {
  body: string;
  time: Date;
} | null = null;
const url = "https://blockchain.info/stats?format=json";

export async function get(req: any, res: any) {
  // Only cache the file for 1 day.
  if (!cache || cache.time.getTime() - new Date().getTime() > 86400 * 1000) {
    const response = await fetch(url);
    const body = await response.text();
    console.log("Fetched ", url);
    const time = new Date();
    cache = { body, time };
  }

  res.writeHead(200, {
    "Content-Type": "application/xml",
    "Last-Modified": cache.time.toUTCString(),
  });
  res.end(cache.body);
}
