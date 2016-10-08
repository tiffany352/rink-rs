// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(rustc_macro)]

extern crate rink;
extern crate iron;
extern crate router;
extern crate params;
extern crate handlebars;
extern crate handlebars_iron;
extern crate staticfile;
extern crate mount;
extern crate ipc_channel;
extern crate libc;
extern crate rustc_serialize;
extern crate serde;
extern crate serde_json;
extern crate limiter;
extern crate logger;
#[macro_use]
extern crate serde_derive;

pub mod worker;

use iron::prelude::*;
use iron::status;
use router::Router;
use iron::AfterMiddleware;
use iron::headers;
use iron::modifiers::Header;
use handlebars::Handlebars;
use handlebars_iron::{HandlebarsEngine, DirectorySource, Template};
use mount::Mount;
use staticfile::Static;
use std::collections::BTreeMap;
use params::{Params, Value};
use std::env;
use worker::{eval_text, eval_json};
use limiter::RequestLimit;
use logger::Logger;
use rustc_serialize::json::ToJson;

fn root(req: &mut Request) -> IronResult<Response> {
    let mut data = BTreeMap::new();

    let map = req.get_ref::<Params>().unwrap();
    match map.find(&["q"]) {
        Some(&Value::String(ref query)) if query == "" => (),
        Some(&Value::String(ref query)) => {
            let mut reply = eval_json(query);
            reply.as_object_mut().unwrap().insert("input".to_owned(), query.to_json());
            println!("{}", reply.pretty());
            data.insert("queries".to_owned(), vec![reply].to_json());
            data.insert("title".to_owned(), query.to_json());
        },
        _ => (),
    };

    if data.len() == 0 {
        data.insert("main-page".to_owned(), true.to_json());
    }

    Ok(Response::with((status::Ok, Template::new("index", data))))
}

struct ErrorMiddleware;

impl AfterMiddleware for ErrorMiddleware {
    fn catch(&self, _req: &mut Request, err: IronError) -> IronResult<Response> {
        let mut data = BTreeMap::new();
        let mut error = BTreeMap::new();
        if let Some(status) = err.response.status {
            error.insert("status".to_owned(), format!("{}", status));
            data.insert("title".to_owned(), format!("{}", status).to_json());
        }
        error.insert("message".to_owned(), format!("{}", err.error));
        data.insert("error".to_owned(), error.to_json());
        println!("{:#?}", data);
        Ok(err.response.set(Template::new("index", data)))
    }
}

fn api(req: &mut Request) -> IronResult<Response> {
    let acao = Header(headers::AccessControlAllowOrigin::Any);

    let map = req.get_ref::<Params>().unwrap();
    let query = match map.find(&["query"]) {
        Some(&Value::String(ref query)) => query,
        _ => return Ok(Response::with((acao, status::BadRequest))),
    };

    let reply = eval_text(query);

    Ok(Response::with((acao, status::Ok, reply)))
}

fn ifnot1helper(
    c: &handlebars::Context,
    h: &handlebars::Helper,
    r: &Handlebars,
    rc: &mut handlebars::RenderContext
) -> Result<(), handlebars::RenderError> {
    use handlebars::RenderError;
    use handlebars::Renderable;

    let param = try!(h.param(0)
                     .ok_or_else(|| RenderError::new("Param not found for helper \"ifnot1\"")));
    let param = param.value();

    let value =
        param.as_string().map(|x| x != "1").unwrap_or(true) &&
        param.as_i64().map(|x| x != 1).unwrap_or(true) &&
        param.as_u64().map(|x| x != 1).unwrap_or(true) &&
        param.as_f64().map(|x| x != 1.0).unwrap_or(true);

    let tmpl = if value {
        h.template()
    } else {
        h.inverse()
    };
    match tmpl {
        Some(ref t) => t.render(c, r, rc),
        None => Ok(()),
    }
}

fn main() {
    let mut args = env::args();
    args.next();
    let first = args.next();
    if first.as_ref().map(|x| x == "--sandbox").unwrap_or(false) {
        let server = args.next().unwrap();
        let query = args.next().unwrap();
        worker::worker(&server, &query);
    }

    let (logger_before, logger_after) = Logger::new(None);

    let mut mount = Mount::new();

    let mut router = Router::new();
    router.get("/", root, "root");
    router.get("/api", api, "api");
    mount.mount("/", router);

    mount.mount("/static", Static::new("./static/"));

    let mut chain = Chain::new(mount);

    let mut hb = Handlebars::new();
    hb.register_helper("ifnot1", Box::new(ifnot1helper));
    let mut hbse = HandlebarsEngine::from(hb);
    hbse.add(Box::new(DirectorySource::new("./templates/", ".hbs")));
    // load templates from all registered sources
    if let Err(r) = hbse.reload() {
        panic!("{}", r);
    }

    let limiter = RequestLimit::new(5000, 5000);

    chain.link_before(logger_before);
    chain.link_before(limiter);
    chain.link_after(ErrorMiddleware);
    chain.link_after(hbse);
    chain.link_after(logger_after);
    let addr = first.as_ref().map(|x| &**x).unwrap_or("localhost:8000");
    Iron::new(chain).http(addr).unwrap();
}
