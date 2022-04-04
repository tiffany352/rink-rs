use crate::{
    loader::gnu_units,
    output::{QueryError, QueryReply},
    parsing::text_query,
    Context,
};

#[cfg(feature = "gpl")]
pub static DEFAULT_FILE: Option<&'static str> = Some(include_str!("../definitions.units"));
#[cfg(not(feature = "gpl"))]
pub static DEFAULT_FILE: Option<&'static str> = None;

pub static DATES_FILE: &str = include_str!("../datepatterns.txt");
pub static CURRENCY_FILE: &str = include_str!("../currency.units");

pub fn eval(ctx: &mut Context, line: &str) -> Result<QueryReply, QueryError> {
    ctx.update_time();
    let mut iter = text_query::TokenIterator::new(line.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    let res = ctx.eval_query(&expr)?;
    if ctx.save_previous_result {
        if let QueryReply::Number(ref number_parts) = res {
            if let Some(ref raw) = number_parts.raw_value {
                ctx.previous_result = Some(raw.clone());
            }
        }
    }
    Ok(res)
}

/// A version of eval() that converts results and errors into strings.
pub fn one_line(ctx: &mut Context, line: &str) -> Result<String, String> {
    eval(ctx, line)
        .as_ref()
        .map(ToString::to_string)
        .map_err(ToString::to_string)
}

/// Tries to create a context that has core definitions only (contents
/// of definitions.units), will fail if the GPL feature isn't enabled.
/// Mainly intended for unit testing.
pub fn simple_context() -> Result<Context, String> {
    let units = match DEFAULT_FILE {
        Some(units) => units,
        None => return Err("GPL feature not enabled, cannot create simple context.".to_owned()),
    };

    let mut iter = gnu_units::TokenIterator::new(&*units).peekable();
    let units = gnu_units::parse(&mut iter);

    let dates = crate::parsing::datetime::parse_datefile(DATES_FILE);

    let mut ctx = Context::new();
    ctx.load(units);
    ctx.load_dates(dates);
    Ok(ctx)
}
