use super::*;
use {
    self::terms::expressions,
    super::items::{at_item, sections::at_section},
};

pub(crate) fn formal_param_list(p: &mut Parser<'_>) {
    let m = p.start();
    formal_comma_sep(p, formal_param);
    m.complete(p, FORMAL_PARAM_LIST);
}

fn formal_param(p: &mut Parser<'_>) -> Result<(), &'static str> {
    let m = p.start();
    let ty = types::type_ref(p);
    match p.current() {
        VAR => {
            if let Err(msg) = ty {
                p.error(msg);
            }
            p.bump(VAR);
            attributes::attributes_opt(p);
        }
        T!['['] => {
            if let Err(msg) = ty {
                p.error(msg);
            }
            attributes::attribute_list(p);
            p.eat(VAR);
        }
        _ => {
            if let Err(e) = ty {
                m.abandon(p);
                return Err(e);
            }
        }
    }
    m.complete(p, FORMAL_PARAM);
    Ok(())
}

fn formal_arg(p: &mut Parser<'_>) -> Result<(), &'static str> {
    let m = p.start();
    let ty = types::type_ref(p);
    match p.current() {
        VAR => {
            if let Err(msg) = ty {
                p.error(msg);
            }
            p.bump(VAR);
            if p.eat(T![=]) && expressions::expr(p).is_none() {
                p.error("expected expression");
            }
            attributes::attributes_opt(p);
        }
        T!['['] => {
            if let Err(msg) = ty {
                p.error(msg);
            }
            attributes::attribute_list(p);
            if p.eat(VAR) && p.eat(T![=]) && expressions::expr(p).is_none() {
                p.error("expected expression");
            }
        }
        _ => {
            if let Err(e) = ty {
                m.abandon(p);
                return Err(e);
            }
        }
    }
    m.complete(p, FORMAL_ARG);
    Ok(())
}

fn formal_comma_sep(
    p: &mut Parser<'_>,
    param: impl Fn(&mut Parser<'_>) -> Result<(), &'static str>,
) {
    p.bump(T!['(']);
    while !p.at(EOF) && !p.at(T![')']) {
        let ellipsis = p.eat(T![...]);
        let param_result = param(p);
        if !p.at(T![')']) {
            if ellipsis {
                p.error("`...` must be the last parameter")
            }

            if p.eat(T![,]) {
                if p.at(T![')']) {
                    p.error("unexpected trailing comma");
                    break;
                }
            } else {
                let msg = param_result.err().unwrap_or("expected `,` or `)`");
                // test_err formal_params_invalid
                // class pp predicates p : (a, b +). end class
                p.error_until(msg, |p| p.at_ts([T![,], T![')']]));
                p.eat(T![,]);
            }
        }
    }
    p.expect(T![')']);
}

pub(crate) fn formal_arg_list(p: &mut Parser<'_>, at_end: impl Fn(&Parser<'_>) -> bool) {
    let m = p.start();
    let mut any = false;
    while !p.at(EOF) && !at_end(p) {
        any = true;
        let formal_arg = formal_arg(p);
        if at_end(p) {
            if let Err(err) = formal_arg {
                p.error(err);
            }
            break;
        }
        if !p.eat(T![,]) {
            let err = match formal_arg {
                Ok(_) => format!("expected `,`, found {:?}", p.current()),
                Err(err) => err.into(),
            };
            p.error_until(err, |p| at_end(p) || p.at(T![,]) || at_section(p) || at_item(p));
            p.eat(T![,]);
        } else if let Err(err) = formal_arg {
            p.error(err);
        }
    }
    m.complete_if(p, FORMAL_ARG_LIST, any);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_ok;
    use expect_test::expect;

    #[test]
    fn type_param_ok_0() {
        parse_ok!(
            formal_arg,
            "V",
            expect![[r#"
                SOURCE_FILE
                  FORMAL_ARG
                    TYPE
                      VAR_TYPE
                        VAR "V"
            "#]]
        );
    }

    #[test]
    fn type_param_ok_1() {
        parse_ok!(
            formal_arg,
            "a* V",
            expect![[r#"
                SOURCE_FILE
                  FORMAL_ARG
                    TYPE
                      LIST_TYPE
                        TYPE
                          REF_TERM
                            IDENT "a"
                        STAR "*"
                    WHITESPACE " "
                    VAR "V"
            "#]]
        );
    }

    #[test]
    fn type_param_ok_2() {
        parse_ok!(
            formal_arg,
            "@T _",
            expect![[r#"
                SOURCE_FILE
                  FORMAL_ARG
                    TYPE
                      SCOPE_TYPE
                        AT "@"
                        VAR "T"
                    WHITESPACE " "
                    VAR "_"
            "#]]
        );
    }
}
