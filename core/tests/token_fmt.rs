// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::borrow::Cow;

use rink_core::output::fmt::{
    FmtToken::{self, *},
    Span, TokenFmt,
};
use rink_core::parsing::text_query;
use rink_core::*;
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub enum FlatSpan<'a> {
    Content { text: Cow<'a, str>, token: FmtToken },
    // Span's Child is a dyn trait, which can't be compared with Eq.
    Child(Vec<FlatSpan<'a>>),
}

impl<'a> From<Span<'a>> for FlatSpan<'a> {
    fn from(span: Span<'a>) -> Self {
        match span {
            Span::Content { text, token } => FlatSpan::Content { text, token },
            Span::Child(child) => {
                FlatSpan::Child(child.to_spans().into_iter().map(Into::into).collect())
            }
        }
    }
}

impl<'a> fmt::Debug for FlatSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatSpan::Content { text, token } => write!(f, "({:?}, {:?})", text, token),
            FlatSpan::Child(obj) => obj.fmt(f),
        }
    }
}

fn s(text: &'static str, token: FmtToken) -> FlatSpan<'static> {
    FlatSpan::Content {
        text: text.into(),
        token,
    }
}

fn child(children: Vec<FlatSpan<'static>>) -> FlatSpan<'static> {
    FlatSpan::Child(children)
}

thread_local! {
    static CONTEXT: Context = {
        let mut ctx = simple_context().unwrap();
        ctx.use_humanize = false;
        ctx
    };
}

fn test(input: &str, output: &[FlatSpan<'static>]) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_query(&expr);
        let res = match res {
            Ok(ref v) => v.to_spans(),
            Err(ref v) => v.to_spans(),
        };
        let res = res.into_iter().map(FlatSpan::from).collect::<Vec<_>>();
        similar_asserts::assert_eq!(res, output);
    });
}

#[test]
fn test_number() {
    test(
        "1 meter",
        &[
            s("1", Number),
            s(" ", Plain),
            s("meter", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("length", Quantity),
            s(")", Plain),
        ],
    );
}

#[test]
fn test_date() {
    test(
        "#jan 01, 2000#",
        &[s("2000-01-01 00:00:00 +00:00", DateTime)],
    );
}

#[test]
fn test_substance() {
    test(
        "light",
        &[
            s("light", Unit),
            s(": ", Plain),
            s("", ListBegin),
            child(vec![
                s("speed", PropName),
                s(" = ", Plain),
                child(vec![
                    s("299792458", Number),
                    s(" ", Plain),
                    s("meter", Unit),
                    s(" /", Plain),
                    s(" ", Plain),
                    s("second", Unit),
                    s(" ", Plain),
                    s("(", Plain),
                    s("velocity", Quantity),
                    s(")", Plain),
                ]),
                s(". ", Plain),
                child(vec![
                    s("Speed that light travels in a vacuum. Defined as exactly 299 792 458 m/s. 26th CGPM (2018, Resolution 1; CR, 210).", DocString),
                ]),
            ]),
        ],
    );
}

#[test]
fn test_duration() {
    test(
        "#jan 02, 2001# - #jan 01, 2001#",
        &[
            child(vec![s("1", Number), s(" ", Plain), s("day", Unit)]),
            s(", ", Plain),
            child(vec![s("0", Number), s(" ", Plain), s("second", Unit)]),
            s(" (", Plain),
            s("time", Quantity),
            s(")", Plain),
        ],
    );
}

const METER_DOC: &'static str = "The metre, symbol m, is the SI unit of length. It is defined by taking the fixed numerical value of the speed of light in vacuum c to be 299 792 458 when expressed in the unit m/s, where the second is defined in terms of Δν_Cs. 26th CGPM (2018, CR; 211)";

#[test]
fn test_definition() {
    test(
        "foot",
        &[
            s("Definition: ", Plain),
            s("foot", Unit),
            s(" = ", Plain),
            s("12 inch", Plain),
            s(" = ", Plain),
            s("304.8", Number),
            s(" ", Plain),
            s("millimeter", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("length", Quantity),
            s("; ", Plain),
            s("m", Unit),
            s(")", Plain),
            s(". ", Plain),
            child(vec![
                s("International yard and pound, since ", DocString),
                s("1959-07-01", DateTime),
                s(".", DocString),
            ]),
        ],
    );
    test(
        "meter",
        &[
            s("Definition: ", Plain),
            s("meter", Unit),
            s(" = ", Plain),
            s("base unit of length", Plain),
            s(". ", Plain),
            child(vec![s(METER_DOC, DocString)]),
        ],
    );
}

#[test]
fn test_conversion() {
    test(
        "meter to foot",
        &[
            s("1250/381", Number),
            s(", approx. ", Plain),
            s("3.280839", Number),
            s(" ", Plain),
            s("foot", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("length", Quantity),
            s(")", Plain),
        ],
    );

    test(
        "meter to 2 foot",
        &[
            s("625/381", Number),
            s(", approx. ", Plain),
            s("1.640419", Number),
            s(" ", Plain),
            s("* ", Plain),
            s("2", Number),
            s(" ", Plain),
            s("foot", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("length", Quantity),
            s(")", Plain),
        ],
    );

    test(
        "meter to 1|5 foot",
        &[
            s("6250/381", Number),
            s(", approx. ", Plain),
            s("16.40419", Number),
            s(" ", Plain),
            s("foot", Unit),
            s(" /", Plain),
            s(" ", Plain),
            s("5", Number),
            s(" ", Plain),
            s("(", Plain),
            s("length", Quantity),
            s(")", Plain),
        ],
    );
}

#[test]
fn test_factorize() {
    test(
        "factorize velocity",
        &[
            s("Factorizations: ", ListBegin),
            child(vec![s("velocity", Unit)]),
            s("; ", ListSep),
            child(vec![
                s("acceleration", Unit),
                s(" ", Plain),
                s("time", Unit),
            ]),
            s("; ", ListSep),
            child(vec![
                s("flow_rate", Unit),
                s(" ", Plain),
                s("fuel_efficiency", Unit),
            ]),
            s("; ", ListSep),
            child(vec![s("frequency", Unit), s(" ", Plain), s("length", Unit)]),
            s("; ", ListSep),
            child(vec![
                s("jerk", Unit),
                s(" ", Plain),
                s("time", Unit),
                s("^2", Pow),
            ]),
        ],
    );
}

#[test]
fn test_units_for() {
    test(
        "units for magnetic_flux",
        &[
            s("Units for ", Plain),
            s("kg m^2 / A s^2", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("magnetic_flux", Quantity),
            s(")", Plain),
            s(": ", ListBegin),
            child(vec![
                s("CGS Units", Plain),
                s(": ", ListBegin),
                s("kappline", Unit),
                s(", ", ListSep),
                s("maxwell", Unit),
                s(", ", ListSep),
                s("statmaxwell", Unit),
                s(", ", ListSep),
                s("unitpole", Unit),
            ]),
            s("; ", ListSep),
            child(vec![
                s("SI Derived Units", Plain),
                s(": ", ListBegin),
                s("weber", Unit),
            ]),
        ],
    );
}

#[test]
fn test_unit_list() {
    test(
        "meter to ft;inch;thou",
        &[
            s("", ListBegin),
            child(vec![s("3", Number), s(" ", Plain), s("foot", Unit)]),
            s(", ", ListSep),
            child(vec![s("3", Number), s(" ", Plain), s("inch", Unit)]),
            s(", ", ListSep),
            child(vec![s("370.0787", Number), s(" ", Plain), s("thou", Unit)]),
            s(" (", Plain),
            s("length", Quantity),
            s(")", Plain),
        ],
    );
}

#[test]
fn test_search() {
    test(
        "search baz",
        &[
            s("Search results: ", ListBegin),
            s("bar", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("pressure", Quantity),
            s(")", Plain),
            s(", ", ListSep),
            s("bag", Unit),
            s(" ", Plain),
            s("(", Plain),
            s("volume", Quantity),
            s(")", Plain),
        ],
    );
}

#[test]
fn test_doclinks() {
    test("floppy", &[
        s("Definition: ", Plain),
        s("floppydisk", Unit),
        s(" = ", Plain),
        s("1440 KiB", Plain),
        s(" = ", Plain),
        s("1.47456", Number),
        s(" ", Plain),
        s("megabyte", Unit),
        s(" ", Plain),
        s("(", Plain),
        s("information", Quantity),
        s("; ", Plain),
        s("bit", Unit),
        s(")", Plain),
        s(". ", Plain),
        child(vec![
        s("The common 3.5 inch floppy disk in \"1.44 Meg\" format. The 1.44 comes from mixing decimal and binary prefixes (1000*1024 bytes). Equal to 512 B x 80 tracks x 18 sectors x 2 sides. Source: ", DocString),
        s("http://www.manmrk.net/tutorials/DOS/PSBOOK/book4/floppyd.htm", Link),
        ])
    ]);
}
