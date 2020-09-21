import { Request, Response as ExpressResponse } from "express";
import createEtag from "etag";

interface CachedValue {
  body: string;
  time: Date;
  etag: string;
}

export default function cached(
  cacheTimeMs: number,
  target: () => Promise<unknown>
) {
  let cache: CachedValue | null = null;
  return async (req: Request, res: ExpressResponse) => {
    if (!cache || cache.time.getTime() - new Date().getTime() > cacheTimeMs) {
      const value = await target();
      const body = JSON.stringify(value);
      const time = new Date();
      const etag = createEtag(body);
      cache = { body, etag, time };
    }

    res.set({
      ETag: cache.etag,
      "Content-Type": "application/json",
      "Last-Modified": cache.time.toUTCString(),
    });
    res.send(cache.body);
  };
}
