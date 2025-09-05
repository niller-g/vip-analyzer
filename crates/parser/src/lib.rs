mod event;
mod grammar;
mod input;
mod lexed_str;
mod output;
mod parser;
mod shortcuts;
mod syntax_kind;
mod token_set;

pub use T_ as T;

#[cfg(any(test, feature = "rast-parser"))]
mod tests;

#[cfg(feature = "rast-parser")]
pub use tests::parse_rast;

pub(crate) use token_set::TokenSet;

pub use crate::{
    input::Input,
    lexed_str::LexedStr,
    output::{Output, Step},
    shortcuts::StrStep,
    syntax_kind::SyntaxKind,
};

pub fn parse_input(input: &Input) -> Output {
    parse(grammar::source_file, input)
}

fn parse(entry_point: fn(&'_ mut parser::Parser<'_>), input: &Input) -> Output {
    let _p = tracing::info_span!("TopEntryPoint::parse").entered();
    let mut p = parser::Parser::new(input);
    entry_point(&mut p);
    let events = p.finish();
    let res = event::process(events);

    if cfg!(debug_assertions) {
        let mut depth = 0;
        let mut first = true;
        for step in res.iter() {
            assert!(depth > 0 || first);
            first = false;
            match step {
                Step::Enter { .. } => depth += 1,
                Step::Exit => depth -= 1,
                Step::Token { .. } | Step::Error { .. } => (),
            }
        }
        assert!(!first, "no tree at all");
        assert_eq!(depth, 0, "unbalanced tree");
    }

    res
}

#[macro_export]
macro_rules! test_ok {
    ($func:expr, $text:expr, $fn_name:ident) => {
        #[test]
        fn $fn_name() {
            let (_, errors) = $crate::test_parser!($func, $text);
            assert!(errors.is_empty(), "unexpected errors:\n{errors:?}");
        }
    };
}

#[macro_export]
macro_rules! test_panic {
    ($func:expr, $text:expr, $fn_name:ident) => {
        #[should_panic]
        #[test]
        fn $fn_name() {
            let (actual, errors) = $crate::test_parser!($func, $text);
            assert!(errors.is_empty(), "No errors when expected:\n{actual}");
        }
    };
}
