use std::fs::File;
use std::time::Duration;
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
                            Expr::Frac(Box::new(Expr::Const("1".to_owned(), None, None)),
                                       Box::new(Expr::Const(integer, frac, None))),
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

pub fn load() -> Result<Defs, String> {
    ::cached("currency.xml", URL, Duration::from_secs(23*60*60)).and_then(parse)
}
