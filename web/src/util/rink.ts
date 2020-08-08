import { QueryResult } from "./reply";

export interface Query {
  getExpr(): any;
}

export interface Context {
  setTime(date: Date): void;
  eval(query: Query): QueryResult;
  loadCurrency(ecb: string, btc: string): void;
  loadBtc(file: string): void;
}

let resolveRink: (rink: any) => void;
const promise = new Promise((resolve, reject) => (resolveRink = resolve));

function withTimeout<T>(timeoutMs: number, promise: Promise<T>): Promise<T> {
  return new Promise((resolve, reject) => {
    let live = true;
    promise
      .then(resolve)
      .catch(reject)
      .finally(() => {
        live = false;
      });
    setTimeout(() => {
      if (live) {
        reject(`Timed out after ${timeoutMs}ms.`);
      }
    }, timeoutMs);
  });
}

export default class Rink {
  rink: any;

  constructor(rink: any) {
    this.rink = rink;
  }

  static setRink(rink: any): void {
    resolveRink(rink);
  }

  static async getRink(): Promise<Rink> {
    let rink = await promise;
    return new Rink(rink);
  }

  parse(input: string): Query {
    return new this.rink.Query(input);
  }

  createContext(): Context {
    return new this.rink.Context();
  }

  async createFullContext(fetchFunc: typeof fetch): Promise<Context> {
    const context = this.createContext();
    try {
      const [currency, btc] = await withTimeout(
        2000,
        Promise.all([
          fetchFunc("data/currency.xml").then((res) => res.text()),
          fetchFunc("data/btc.json").then((res) => res.text()),
        ])
      );
      context.loadCurrency(currency, btc);
    } catch (err) {
      console.log("Loading currency data failed:", err);
    }
    return context;
  }
}
