use std::fs::File;
use std::time::{SystemTime, Duration};
use std::fmt::Display;
use hyper::Client;
use hyper::status::StatusCode;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::fs::create_dir_all;
use xml::EventReader;
use xml::reader::XmlEvent;
use ast::{Defs, Def, Expr};
use std::rc::Rc;

static URL: &'static str = "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml";

pub fn parse(f: File) -> Result<Defs, String> {
    let reader = EventReader::new(f);
    let mut out = vec![];
    for ev in reader {
        match ev {
            Ok(XmlEvent::StartElement {
                ref name,
                attributes: ref attrs,
                ..
            }) if name.local_name == "Cube" => {
                let mut currency = None;
                let mut rate = None;
                for attr in attrs {
                    match &*attr.name.local_name {
                        "currency" =>
                            currency = Some(&*attr.value),
                        "rate" =>
                            rate = Some(&*attr.value),
                        _ => (),
                    }
                }
                if let (Some(currency), Some(rate)) = (currency, rate) {
                    let mut iter = rate.split(".");
                    let integer = iter.next().unwrap().to_owned();
                    let frac = iter.next().map(|x| x.to_owned());
                    out.push((currency.to_owned(), Rc::new(Def::Unit(
                        Expr::Mul(vec![
                            Expr::Const(integer, frac, None),
                            Expr::Unit("EUR".to_string())
                        ])))));
                }
            },
            Err(e) => return Err(format!("{}", e)),
            _ => (),
        }
    }
    Ok(Defs {
        defs: out
    })
}

fn path() -> Result<PathBuf, String> {
    let mut path = try!(::config_dir());
    path.push("rink/currency.xml");
    Ok(path)
}

fn download() -> Result<(), String> {
    try!(create_dir_all(try!(path()).parent().unwrap()).map_err(|x| format!("{}", x)));
    let mut f = try!(File::create(try!(path())).map_err(|x| format!("{}", x)));

    let client = Client::new();
    let mut res = try!(client.get(URL).send().map_err(|x| format!("{}", x)));
    if res.status != StatusCode::Ok {
        return Err(format!("Request failed with status code {}", res.status))
    }
    let mut buf = vec![0; 8192];
    loop {
        match res.read(&mut buf) {
            Ok(0) => break,
            Ok(n) => {
                try!(f.write(&buf[..n]).map_err(|x| format!("{}", x)));
            },
            Err(e) => return Err(format!("{}", e))
        }
    }
    try!(f.sync_all().map_err(|x| format!("{}", x)));
    Ok(())
}

fn open() -> Result<Defs, String> {
    fn ts<T:Display>(x: T) -> String {
        format!("{}", x)
    }
    let f = try!(File::open(try!(path())).map_err(ts));
    let stats = try!(f.metadata().map_err(ts));
    let mtime = try!(stats.modified().map_err(ts));
    let now = SystemTime::now();
    let elapsed = try!(now.duration_since(mtime).map_err(ts));
    if elapsed > Duration::from_secs(23*60*60) {
        return Err(format!("File is out of date"))
    }
    parse(f)
}

pub fn load() -> Result<Defs, String> {
    open().or_else(|_| download().and_then(|()| open()))
}
