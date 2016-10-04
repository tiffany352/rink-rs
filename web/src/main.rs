// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
extern crate iron;
extern crate router;
extern crate params;

use iron::prelude::*;
use iron::status;
use router::Router;
use iron::headers;
use iron::modifiers::Header;
use iron::mime::Mime;

static TEMPLATE: &'static str = r##"
<html>
 <head>
  <title>Rink</title>
 </head>
 <body>
  <form action="/api" method="POST" target="_self">
   <input name="query" type="text" />
   <input type="submit" />
  </form>
 </body>
</html>
"##;

fn root(_req: &mut Request) -> IronResult<Response> {
    let mime: Mime = "text/html".parse().unwrap();
    Ok(Response::with((status::Ok, TEMPLATE, mime)))
}

fn api(req: &mut Request) -> IronResult<Response> {
    use params::{Params, Value};

    let map = req.get_ref::<Params>().unwrap();

    let acao = Header(headers::AccessControlAllowOrigin::Any);

    let query = match map.find(&["query"]) {
        Some(&Value::String(ref query)) => query,
        _ => return Ok(Response::with((acao, status::BadRequest))),
    };

    let reply = rink::one_line_sandbox(query);

    Ok(Response::with((acao, status::Ok, reply)))
}

fn main() {
    let mut router = Router::new();
    router.get("/", root, "root");
    router.get("/api", api, "api");
    Iron::new(router).http("localhost:8000").unwrap();
}
