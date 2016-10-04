// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
extern crate iron;
extern crate router;
extern crate params;
extern crate handlebars;
extern crate handlebars_iron;

use iron::prelude::*;
use iron::status;
use router::Router;
use iron::headers;
use iron::modifiers::Header;
use handlebars_iron::{HandlebarsEngine, DirectorySource, Template};

fn root(_req: &mut Request) -> IronResult<Response> {
    Ok(Response::with(Template::new("index", ())))
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

    let mut chain = Chain::new(router);
    let mut hbse = HandlebarsEngine::new();
    hbse.add(Box::new(DirectorySource::new("./templates/", ".hbs")));

    // load templates from all registered sources
    if let Err(r) = hbse.reload() {
        panic!("{}", r);
    }

    chain.link_after(hbse);
    Iron::new(chain).http("localhost:8000").unwrap();
}
