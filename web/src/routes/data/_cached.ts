import { Request, Response as ExpressResponse } from "express";
import createEtag from "etag";

interface CachedValue {
  status: number;
  time: Date;
  etag: string;
  body: string;
}

export default function cached(
  cacheTimeMs: number,
  target: () => Promise<unknown>
) {
  let cache: CachedValue | null = null;
  return async (req: Request, res: ExpressResponse) => {
    if (!cache || cache.time.getTime() - new Date().getTime() > cacheTimeMs) {
      try {
        const value = await target();
        const body = JSON.stringify(value);
        const time = new Date();
        const etag = createEtag(body);
        cache = { status: 200, body, etag, time };
      } catch (err) {
        console.log("Error updating cached resource:", err);
        const time = new Date();
        const body = JSON.stringify({
          code: err.code,
          message: err.toString(),
          date: time.toISOString(),
        });
        const etag = createEtag(body);
        cache = {
          status: 503,
          body,
          etag,
          time,
        };
      }
    }

    res.status(cache.status).set({
      ETag: cache.etag,
      "Content-Type": "application/json",
      "Last-Modified": cache.time.toUTCString(),
    });
    res.send(cache.body);
  };
}
