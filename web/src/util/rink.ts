export interface Query {
  getExpr(): any;
}

export interface Context {
  setTime(date: Date): void;
  eval(query: Query): void;
}

let resolveRink: (rink: any) => void;
const promise = new Promise((resolve, reject) => (resolveRink = resolve));

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
}
