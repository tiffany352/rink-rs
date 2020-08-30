import fetch from "node-fetch";
import { Request, Response } from "express";

let cache: {
  body: string;
  time: Date;
} | null = null;
const url = "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml";

export async function get(req: Request, res: Response) {
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
  res.end(cache?.body);
}
