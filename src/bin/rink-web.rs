#[cfg(feature = "web")]
extern crate hyper;
#[cfg(feature = "web")]
extern crate url;
#[cfg(feature = "web")]
extern crate rink;

#[cfg(feature = "web")]
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

#[cfg(feature = "web")]
fn main() {
    use hyper;
    use hyper::status::StatusCode;
    use hyper::server::{Server, Request, Response};
    use hyper::header;
    use hyper::mime::{Mime, TopLevel, SubLevel};
    use std::env::args;
    use std::io::{Read, Write};
    use url::form_urlencoded;
    use std::sync::mpsc;
    use std::sync::Mutex;
    use std::thread;

    let (tx, rx) = mpsc::channel::<(String, mpsc::Sender<String>)>();
    thread::spawn(move || {
        let mut ctx = rink::load().unwrap();

        while let Ok((req, tx)) = rx.recv() {
            let reply = match rink::one_line(&mut ctx, &*req) {
                Ok(v) => v,
                Err(v) => v
            };
            tx.send(reply).unwrap();
        }
    });
    let tx = Mutex::new(tx);

    let req = move |mut req: Request, mut res: Response| {
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
                let (tx2, rx2) = mpsc::channel();
                tx.lock().unwrap().send((input, tx2)).unwrap();
                let reply = rx2.recv().unwrap();
                write!(&mut res.start().unwrap(), "{}", reply).unwrap();
            },
            _ => *res.status_mut() = StatusCode::MethodNotAllowed
        }
    };

    let port = args().nth(1).map(|x| x.parse::<u16>().expect("Invalid port number")).unwrap_or(8000);
    Server::http(&*format!("127.0.0.1:{}", port)).unwrap().handle(req).unwrap();
}

#[cfg(not(feature = "web"))]
fn main() {
    println!("Rink was not compiled with web support.");
}
