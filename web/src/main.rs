// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(proc_macro)]

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
extern crate url;
extern crate toml;
extern crate serde_derive;

pub mod worker;

use iron::prelude::*;
use iron::status;
use router::Router;
use iron::AfterMiddleware;
use iron::headers;
use iron::modifiers::Header;
use iron::mime::Mime;
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
use rustc_serialize::json::{ToJson, Json};
use std::sync::Arc;
use std::fs::File;

struct Rink {
    config: Json,
}

fn root(rink: &Rink, req: &mut Request) -> IronResult<Response> {
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

    data.insert("config".to_owned(), rink.config.to_json());

    Ok(Response::with((status::Ok, Template::new("index", data))))
}

struct ErrorMiddleware(Arc<Rink>);

impl AfterMiddleware for ErrorMiddleware {
    fn catch(&self, _req: &mut Request, err: IronError) -> IronResult<Response> {
        let mut data = BTreeMap::new();
        let mut error = BTreeMap::new();
        if let Some(status) = err.response.status {
            error.insert("status".to_owned(), status.to_string());
            data.insert("title".to_owned(), status.to_string().to_json());
        }
        error.insert("message".to_owned(), err.error.to_string());
        data.insert("error".to_owned(), error.to_json());
        data.insert("config".to_owned(), self.0.config.to_json());
        println!("{:#?}", data);
        Ok(err.response.set(Template::new("index", data)))
    }
}

fn api(_rink: &Rink, req: &mut Request) -> IronResult<Response> {
    let acao = Header(headers::AccessControlAllowOrigin::Any);

    let map = req.get_ref::<Params>().unwrap();
    let query = match map.find(&["query"]) {
        Some(&Value::String(ref query)) => query,
        _ => return Ok(Response::with((acao, status::BadRequest))),
    };

    let reply = eval_text(query);

    Ok(Response::with((acao, status::Ok, reply)))
}

fn opensearch(rink: &Rink, _req: &mut Request) -> IronResult<Response> {
    let mime: Mime = "application/opensearchdescription+xml".parse().unwrap();
    let mut data = BTreeMap::new();
    data.insert("config".to_owned(), rink.config.to_json());

    Ok(Response::with((status::Ok, mime, Template::new("opensearch", data))))
}

fn ifnot1helper(
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
        Some(ref t) => t.render(r, rc),
        None => Ok(()),
    }
}

fn urlescapehelper(
    h: &handlebars::Helper,
    r: &Handlebars,
    rc: &mut handlebars::RenderContext
) -> Result<(), handlebars::RenderError> {
    use handlebars::RenderError;
    use handlebars::Renderable;
    use url::percent_encoding::{utf8_percent_encode, QUERY_ENCODE_SET};

    let tmpl = h.template();
    let res = match tmpl {
        Some(ref t) => try!(t.renders(r, rc)),
        None => return Err(RenderError::new("urlescape is a block helper")),
    };
    let res = res.split_whitespace().collect::<Vec<_>>().join(" ");
    let res = utf8_percent_encode(&res, QUERY_ENCODE_SET).collect::<String>();
    let res = res.split("%20").collect::<Vec<_>>().join("+");
    try!(rc.writer.write_all(res.as_bytes()).map_err(
        |e| RenderError::new(&e.to_string())));
    Ok(())
}

#[cfg(feature = "watch")]
fn watch(hbse: &Arc<HandlebarsEngine>) {
    use handlebars_iron::Watchable;
    hbse.watch("./templates/");
}

#[cfg(not(feature = "watch"))]
fn watch(_hbse: &Arc<HandlebarsEngine>) {}

fn main() {
    let mut args = env::args();
    args.next();
    let first = args.next();
    if first.as_ref().map(|x| x == "--sandbox").unwrap_or(false) {
        let server = args.next().unwrap();
        let query = args.next().unwrap();
        worker::worker(&server, &query);
    }

    let config = {
        use std::io::Read;

        let mut file = File::open("rink-web.toml").expect(
            "Config file rink-web.toml does not exist. You \
             must create it with the keys specified in the \
             sample."
        );
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        let res = toml::Parser::new(&buf).parse().unwrap();
        rustc_serialize::json::Json::from_str(
            &serde_json::ser::to_string(&res).unwrap()
        ).unwrap()
    };
    let rink = Arc::new(Rink {
        config,
    });
    let (logger_before, logger_after) = Logger::new(None);

    let mut mount = Mount::new();

    let mut router = Router::new();
    let rink2 = rink.clone();
    router.get("/", move |req: &mut Request| root(&rink2, req), "root");
    let rink2 = rink.clone();
    router.get("/api", move |req: &mut Request| api(&rink2, req), "api");
    let rink2 = rink.clone();
    router.get("/opensearch.xml", move |req: &mut Request| opensearch(&rink2, req), "opensearch.xml");
    mount.mount("/", router);

    mount.mount("/static", Static::new("./static/"));

    let mut chain = Chain::new(mount);

    let mut hb = Handlebars::new();
    hb.register_helper("ifnot1", Box::new(ifnot1helper));
    hb.register_helper("urlescape", Box::new(urlescapehelper));
    let mut hbse = HandlebarsEngine::from(hb);
    hbse.add(Box::new(DirectorySource::new("./templates/", ".hbs")));
    // load templates from all registered sources
    if let Err(r) = hbse.reload() {
        panic!("{}", r);
    }
    let hbse = Arc::new(hbse);
    watch(&hbse);

    let mut limiter = RequestLimit::default();
    limiter.set_max_body_size(5000);
    limiter.set_max_url_length(5000);

    chain.link_before(logger_before);
    chain.link_before(limiter);
    chain.link_after(ErrorMiddleware(rink.clone()));
    chain.link_after(hbse);
    chain.link_after(logger_after);
    let addr = first.as_ref().map(|x| &**x).unwrap_or("localhost:8000");
    Iron::new(chain).http(addr).unwrap();
}
