use rink_core::{
    fmt::{Span, TokenFmt},
    simple_context, text_query, Context,
};

thread_local! {
  static CONTEXT: Context = {
      let mut ctx = simple_context().unwrap();
      ctx.use_humanize = false;
      ctx
  };
}

// Throw away formatting info, converting to a string.
fn write_string<'a>(string: &mut String, obj: &'a dyn TokenFmt<'a>) {
    let spans = obj.to_spans();
    for span in spans {
        match span {
            Span::Content { text, .. } => string.push_str(&text),
            Span::Child(obj) => write_string(string, obj),
        }
    }
}

fn test(input: &str, output: &str) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_outer(&expr);
        let mut string = String::new();
        match res {
            Ok(v) => write_string(&mut string, &v),
            Err(v) => write_string(&mut string, &v),
        };
        assert_eq!(string, output);
    });
}

#[test]
fn correct_whitespace() {
    test("m s", "1 meter second");
    test("kg m / s", "1 kilogram meter / second (impulse)");
}
