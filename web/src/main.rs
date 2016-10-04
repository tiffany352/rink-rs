// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate hyper;
extern crate url;
extern crate rink;

static TEMPLATE: &'static str = r##"
<html>
 <head>
  <title>Rink</title>
 </head>
 <body>
  <form action="#" method="POST" target="_self">
   <input name="value" type="text" />
   <input type="submit" />
  </form>
 </body>
</html>
"##;

fn main() {
    use hyper;
    use hyper::status::StatusCode;
    use hyper::server::{Server, Request, Response};
    use hyper::header;
    use hyper::mime::{Mime, TopLevel, SubLevel};
    use std::env::args;
    use std::io::{Read, Write};
    use url::form_urlencoded;

    let req = move |mut req: Request, mut res: Response| {
        res.headers_mut().set::<header::AccessControlAllowOrigin>(header::AccessControlAllowOrigin::Any);
        match req.method {
            hyper::Get => {
                write!(res.start().unwrap(), "{}", TEMPLATE).unwrap();
            },
            hyper::Post => {
                let mut buf = String::new();
                match req.read_to_string(&mut buf) {
                    Ok(_) => (),
                    Err(e) => {
                        *res.status_mut() = StatusCode::BadRequest;
                        write!(res.start().unwrap(), "{}", e).unwrap();
                        return
                    }
                };
                let is_form = req.headers.get::<header::ContentType>()
                    .map(|x| match x.0 {
                        Mime(TopLevel::Application, SubLevel::WwwFormUrlEncoded, _) => true,
                        _ => false,
                    })
                    .unwrap_or(false);
                let input = if is_form {
                    let mut form = form_urlencoded::parse(buf.as_bytes());
                    let value = form.find(|&(ref k, _)| k == "value");
                    if let Some((_, value)) = value {
                        value.into_owned()
                    } else {
                        *res.status_mut() = StatusCode::BadRequest;
                        write!(&mut res.start().unwrap(), "Bad form").unwrap();
                        return
                    }
                } else {
                    buf
                };
                let reply = rink::one_line_sandbox(&*input);
                write!(&mut res.start().unwrap(), "{}", reply).unwrap();
            },
            _ => *res.status_mut() = StatusCode::MethodNotAllowed
        }
    };

    let port = args().nth(1).map(|x| x.parse::<u16>().expect("Invalid port number")).unwrap_or(8000);
    Server::http(&*format!("127.0.0.1:{}", port)).unwrap().handle(req).unwrap();
}
