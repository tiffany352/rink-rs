// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
extern crate iron;
extern crate router;
extern crate params;
extern crate handlebars;
extern crate handlebars_iron;
extern crate staticfile;
extern crate mount;

use iron::prelude::*;
use iron::status;
use router::Router;
use iron::headers;
use iron::modifiers::Header;
use handlebars_iron::{HandlebarsEngine, DirectorySource, Template};
use mount::Mount;
use staticfile::Static;
use std::collections::BTreeMap;
use params::{Params, Value};

fn root(req: &mut Request) -> IronResult<Response> {
    let mut data = BTreeMap::new();

    let map = req.get_ref::<Params>().unwrap();
    match map.find(&["q"]) {
        Some(&Value::String(ref query)) => {
            let reply = rink::one_line_sandbox(query);
            data.insert("content".to_owned(), reply);
        },
        _ => (),
    };

    Ok(Response::with((status::Ok, Template::new("index", data))))
}

fn api(req: &mut Request) -> IronResult<Response> {
    let acao = Header(headers::AccessControlAllowOrigin::Any);

    let map = req.get_ref::<Params>().unwrap();
    let query = match map.find(&["query"]) {
        Some(&Value::String(ref query)) => query,
        _ => return Ok(Response::with((acao, status::BadRequest))),
    };

    let reply = rink::one_line_sandbox(query);

    Ok(Response::with((acao, status::Ok, reply)))
}

fn main() {
    let mut mount = Mount::new();

    let mut router = Router::new();
    router.get("/", root, "root");
    router.get("/api", api, "api");
    mount.mount("/", router);

    mount.mount("/static", Static::new("./static/"));

    let mut chain = Chain::new(mount);
    let mut hbse = HandlebarsEngine::new();
    hbse.add(Box::new(DirectorySource::new("./templates/", ".hbs")));

    // load templates from all registered sources
    if let Err(r) = hbse.reload() {
        panic!("{}", r);
    }

    chain.link_after(hbse);
    Iron::new(chain).http("localhost:8000").unwrap();
}
