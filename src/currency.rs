use std::fs::File;
use std::time::{SystemTime, Duration};
use std::fmt::Display;
use hyper::Client;
use hyper::status::StatusCode;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::fs::create_dir_all;

static URL: &'static str = "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml";

pub type Parsed = Vec<(String, f64)>;

pub fn parse(_f: File) -> Parsed {
    unimplemented!()
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

fn open() -> Result<Parsed, String> {
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
    Ok(parse(f))
}

pub fn load() -> Result<Parsed, String> {
    open().or_else(|_| download().and_then(|()| open()))
}
