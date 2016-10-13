// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
extern crate clap;
extern crate encoding;
extern crate lmdb_zero as lmdb;

use clap::{Arg, App};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use encoding::{Encoding, DecoderTrap};
use encoding::all::ISO_8859_1;
use std::io;
use std::cmp::Ordering;
use std::str::FromStr;
use std::fmt;
use std::io::Write;
use std::collections::BTreeMap;
use lmdb::{Database, DatabaseOptions, WriteTransaction, EnvBuilder, put, db};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Rat(i32, i32);

impl FromStr for Rat {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = s.split(".");
        match (i.next(), i.next(), i.next()) {
            (Some(l), None, None) => {
                let l = try!(l.parse().map_err(|x| io::Error::new(io::ErrorKind::Other, x)));
                Ok(Rat(l, 1))
            },
            (Some(l), Some(r), None) => {
                let l: i32 = if l.len() == 0 {
                    0
                } else {
                    try!(l.parse().map_err(|x| io::Error::new(io::ErrorKind::Other, x)))
                };
                let s = r.len();
                let r: i32 = try!(r.parse().map_err(|x| io::Error::new(io::ErrorKind::Other, x)));
                let e = 10i32.pow(s as u32);
                Ok(Rat(l*e + r, e))
            },
            _ => Err(io::Error::new(io::ErrorKind::Other, format!("Expected decimal, got {}", s)))
        }
    }
}

impl PartialOrd for Rat {
    fn partial_cmp(&self, other: &Rat) -> Option<Ordering> {
        let l = self.0 as i64 * other.1 as i64;
        let r = other.0 as i64 * self.1 as i64;
        Some(l.cmp(&r))
    }
}

impl Ord for Rat {
    fn cmp(&self, other: &Rat) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl fmt::Display for Rat {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}.{}", self.0 / self.1, self.0 % self.1)
    }
}

fn main() {
    let matches = App::new("rink-import-usda")
        .version("0.3.2")
        .author("Tiffany Bennett <tiffany@stormbit.net>")
        .about(
            "Imports USDA ARS ASCII-formatted dataset into Rink's \
             native unit definition language."
        )
        .arg(Arg::with_name("INPUTDIR")
             .help("Sets the path to a directory containing the files.")
             .required(true)
             .index(1))
        .arg(Arg::with_name("DB")
             .long("db")
             .value_name("DB")
             .help("Sets the database path")
             .required(true)
             .takes_value(true))
        .get_matches();
    let dir = PathBuf::from(matches.value_of("INPUTDIR").unwrap());
    let db_path = matches.value_of("DB").unwrap();

    let env = unsafe {
        let mut b = EnvBuilder::new().expect("Creating builder");
        b.set_mapsize(1024*1024*100).expect("Setting mapsize");
        b.set_maxdbs(10).expect("Setting maxdbs");
        b.open(
            db_path, lmdb::open::Flags::empty(), 0o600
        ).expect("Opening env")
    };
    let db = Database::open(
        &env, Some("substances"), &DatabaseOptions::new(db::CREATE)
    ).expect("Creating database");

    macro_rules! field_type {
        (A) => { Option<String> };
        (N) => { Option<Rat> }
    }

    macro_rules! table {
        (file: $file:expr, primary_key: ($($key:ident),+), $($name:ident $ty:ident),+) => {{
            #[allow(non_snake_case, dead_code)]
            #[derive(Debug)]
            struct Record {$(
                $name: field_type!($ty),
            )+
                rowid: Option<u64>,
            }

            let mut records = BTreeMap::new();

            let mut file = File::open(dir.join($file)).expect(&format!("Failed to open {}", $file));
            let mut buf = vec![];
            file.read_to_end(&mut buf).unwrap();
            let res = ISO_8859_1.decode(&buf, DecoderTrap::Replace).unwrap();

            let mut rowid = 0;
            for line in res.lines() {
                //let line = line.expect("Malformed line in input");
                let mut fields = line.split("^");
                /*let mut record = BTreeMap::<&'static str, String>::new();

                $({
                    let value = fields.next().expect(&format!(
                        "Truncated record: Expected field {}",
                        stringify!($name)
                    ));
                    let value = value.trim_matches('~');
                    if value.len() > 0 {
                        record.insert(
                            stringify!($name),
                            value.to_owned()
                        );
                    }
                })+;*/

                let record = Record {$(
                    $name: {
                        let value = fields.next().expect(&format!(
                            "Truncated record: Expected field {}",
                            stringify!($name)
                        ));
                        let value = value.trim_matches('~');
                        if value.len() > 0 {
                            Some(value.parse().expect(&format!(
                                "Parsing field {} failed (value = {})",
                                stringify!($name), value
                            )))
                        } else {
                            None
                        }
                    },
                )+
                    rowid: Some(rowid),
                };

                rowid += 1;

                records.insert(
                    ( $(record.$key.clone().unwrap()),+ ),
                    record
                );

                /*records.insert(
                    ( $(record.remove(stringify!($key)).unwrap().parse::<i32>().unwrap()),+ ),
                    record
                );*/
            }

            records
        }}
    }

    let food_des = table! {
        file: "FOOD_DES.txt",
        primary_key: (NDB_No),
        NDB_No N,
        FdGrp_Cd N,
        Long_Desc A,
        Shrt_Desc A,
        ComName A,
        ManufacName A,
        Survey A,
        Ref_desc A,
        Refuse N,
        SciName A,
        N_Factor N,
        Pro_Factor N,
        Fat_Factor N,
        CHO_Factor N
    };

    let nut_data = table! {
        file: "NUT_DATA.txt",
        primary_key: (NDB_No, Nutr_No),
        NDB_No N,
        Nutr_No N,
        Nutr_Val N,
        Num_Data_Pt N,
        Std_Error N,
        Src_Cd A,
        Deriv_Cd A,
        Ref_NDB_No N,
        Add_Nutr_Mar A,
        Num_Studies N,
        Min N,
        Max N,
        DF N,
        Low_EB N,
        Up_EB N,
        Stat_cmt A,
        AddMod_Date A,
        CC A
    };

    let nutr_def = table! {
        file: "NUTR_DEF.txt",
        primary_key: (Nutr_No),
        Nutr_No N,
        Units A,
        Tagname A,
        NutrDesc A,
        Num_Dec A,
        SR_Order N
    };

    let weight = table! {
        file: "WEIGHT.txt",
        primary_key: (NDB_No, Seq),
        NDB_No N,
        Seq N,
        Amount N,
        Msre_Desc A,
        Gm_Wgt N,
        Num_Data_Pts N,
        Std_Dev N
    };

    let footnote = table! {
        file: "FOOTNOTE.txt",
        primary_key: (rowid),
        NDB_No N,
        Footnt_No A,
        Footnt_Typ A,
        Nutr_No N,
        Footnt_Txt A
    };

    let mut properties = BTreeMap::new();
    properties.insert("Energy",   ("food_energy", "by_energy", "mass"));
    properties.insert("Protein",  ("protein", "by_protein", "mass"));
    properties.insert("Water",    ("water", "by_water", "mass"));
    properties.insert("Caffeine", ("caffeine", "by_caffeine", "mass"));
    properties.insert("Sugars",   ("sugars", "by_sugars", "mass"));
    properties.insert("Alcohol",  ("alcohol", "by_alcohol", "mass"));

    let txn = WriteTransaction::new(&env).expect("Transaction");
    {
        let mut access = txn.access();
        for (&ndb, des_record) in &food_des {
            let mut output = vec![];
            for (_, fn_rec) in &footnote {
                if fn_rec.NDB_No == Some(ndb) && fn_rec.Footnt_Typ.as_ref().map(|x| x == "D").unwrap_or(false) {
                    if let Some(ref txt) = fn_rec.Footnt_Txt {
                        writeln!(output, "?? {}", txt).unwrap();
                    } else {
                        println!("{}: Footnote with missing text: {:?}", ndb, fn_rec);
                    }
                }
            }
            let name = if let Some(ref desc) = des_record.Long_Desc {
                desc.chars().map(|c| {
                    c.escape_default().collect::<String>()
                }).collect::<String>()
            } else {
                println!("{}: Missing description", ndb);
                continue;
            };
            writeln!(output, "\"{}\" {{", name).unwrap();
            for (&nutr, nutr_record) in &nutr_def {
                if let Some(nut) = nut_data.get(&(ndb, nutr)) {
                    let (desc, val, units) = match (nutr_record.NutrDesc.as_ref(), nut.Nutr_Val.as_ref(), nutr_record.Units.as_ref()) {
                        (Some(a), Some(b), Some(c)) => (a,b,c),
                        (None, _, _) => {
                            println!("{}: {} missing description", ndb, nutr);
                            continue
                        },
                        (_, None, _) => {
                            println!("{}: {} missing value", ndb, nutr);
                            continue
                        },
                        (_, _, None) => {
                            println!("{}: {} missing units", ndb, nutr);
                            continue
                        },
                    };
                    let (property, input, out) = match properties.get(&**desc) {
                        Some(x) => *x,
                        None => continue
                    };
                    for (_, fn_rec) in &footnote {
                        if {
                            fn_rec.NDB_No == Some(ndb) &&
                                fn_rec.Footnt_Typ.as_ref().map(|x| x == "N").unwrap_or(false) &&
                                fn_rec.Nutr_No == Some(nutr)
                        } {
                            if let Some(ref txt) = fn_rec.Footnt_Txt {
                                writeln!(output, "  ?? {}", txt).unwrap();
                            } else {
                                println!("{}: Footnote for nutr {} missing text: {:?}", ndb, nutr, fn_rec);
                            }
                        }
                    }
                    writeln!(
                        output, "  {} {} {} {} / {} 100 g",
                        property, input, val, units, out
                    ).unwrap();
                }
            }
            for (&(weight_ndb, _seq), wrec) in &weight {
                if weight_ndb != ndb {
                    continue
                }
                let mut mapping = BTreeMap::new();
                mapping.insert("cup", "cup");
                mapping.insert("tbsp", "tbsp");
                mapping.insert("oz", "floz");
                mapping.insert("cubic inch", "inch^3");
                let measure = match wrec.Msre_Desc.as_ref().and_then(|x| mapping.get(&**x)) {
                    Some(&x) => x,
                    None => continue
                };
                writeln!(
                    output, "  density mass {} g / volume {} {}",
                    wrec.Amount.unwrap(),
                    measure,
                    wrec.Gm_Wgt.unwrap()
                ).unwrap();
                break;
            }
            writeln!(output, "}}").unwrap();
            writeln!(output, "").unwrap();

            access.put(&db, &*name, &output, put::Flags::empty())
                .expect("Put");
        }
    }

    txn.commit().expect("Commit");
}
