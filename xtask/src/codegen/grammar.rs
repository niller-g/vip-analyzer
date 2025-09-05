//! This module generates AST datatype used by rust-analyzer.
//!
//! Specifically, it generates the `SyntaxKind` enum and a number of newtype
//! wrappers around `SyntaxNode` which implement `syntax::AstNode`.

#![expect(clippy::disallowed_types)]

use std::fmt::Write;
use std::{
    collections::{BTreeSet, HashSet},
    fs,
};

use either::Either;
use itertools::Itertools;
use proc_macro2::{Punct, Spacing};
use quote::{format_ident, quote};
use stdx::panic_context;
use ungrammar::{Grammar, Rule};

use crate::{
    codegen::{add_preamble, ensure_file_contents, grammar::ast_src::generate_kind_src, reformat},
    project_root,
};

mod ast_src;

use self::ast_src::{AstEnumSrc, AstNodeSrc, AstSrc, Cardinality, Field, KindsSrc};

pub(crate) fn generate(check: bool) {
    let grammar = fs::read_to_string(project_root().join("crates/syntax/vip.ungram"))
        .unwrap()
        .parse()
        .unwrap();
    let ast = lower(&grammar);
    let kinds_src = generate_kind_src(&ast.nodes, &ast.enums, &grammar);

    let syntax_kinds = generate_syntax_kinds(kinds_src);
    let syntax_kinds_file = project_root().join("crates/parser/src/syntax_kind/generated.rs");
    ensure_file_contents(
        crate::flags::CodegenType::Grammar,
        syntax_kinds_file.as_path(),
        &syntax_kinds,
        check,
    );

    let ast_tokens = generate_tokens(&ast);
    let ast_tokens_file = project_root().join("crates/syntax/src/ast/generated/tokens.rs");
    ensure_file_contents(
        crate::flags::CodegenType::Grammar,
        ast_tokens_file.as_path(),
        &ast_tokens,
        check,
    );

    let ast_nodes = generate_nodes(kinds_src, &ast);
    let ast_nodes_file = project_root().join("crates/syntax/src/ast/generated/nodes.rs");
    ensure_file_contents(
        crate::flags::CodegenType::Grammar,
        ast_nodes_file.as_path(),
        &ast_nodes,
        check,
    );
}

fn generate_tokens(grammar: &AstSrc) -> String {
    let tokens = grammar.tokens.iter().map(|token| {
        let name = format_ident!("{}", token);
        let kind = format_ident!("{}", to_upper_snake_case(token));
        quote! {
            pub struct #name {
                pub(crate) syntax: SyntaxToken,
            }
            impl std::fmt::Display for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    std::fmt::Display::fmt(&self.syntax, f)
                }
            }
            impl AstToken for #name {
                fn can_cast(kind: SyntaxKind) -> bool { kind == #kind }
                fn cast(syntax: SyntaxToken) -> Option<Self> {
                    if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                }
                fn syntax(&self) -> &SyntaxToken { &self.syntax }
            }

            impl fmt::Debug for #name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.debug_struct(#token).field("syntax", &self.syntax).finish()
                }
            }
            impl Clone for #name {
                fn clone(&self) -> Self {
                    Self { syntax: self.syntax.clone() }
                }
            }
            impl hash::Hash for #name {
                fn hash<H: hash::Hasher>(&self, state: &mut H) {
                    self.syntax.hash(state);
                }
            }

            impl Eq for #name {}
            impl PartialEq for #name {
                fn eq(&self, other: &Self) -> bool {
                    self.syntax == other.syntax
                }
            }
        }
    });

    add_preamble(
        crate::flags::CodegenType::Grammar,
        reformat(
            quote! {
                use std::{fmt, hash};

                use crate::{SyntaxKind::{self, *}, SyntaxToken, ast::AstToken};

                #(#tokens)*
            }
            .to_string(),
        ),
    )
    .replace("#[derive", "\n#[derive")
}

fn generate_nodes(kinds: KindsSrc, grammar: &AstSrc) -> String {
    let (node_defs, node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .map(|node| {
            let node_str_name = &node.name;
            let name = format_ident!("{}", node.name);
            let kind = format_ident!("{}", to_upper_snake_case(&node.name));
            let traits = node.traits.iter().map(|trait_name| {
                let trait_name = format_ident!("{}", trait_name);
                quote!(impl ast::#trait_name for #name {})
            });

            let methods = node.fields.iter().map(|field| {
                let method_name = format_ident!("{}", field.method_name());
                let ty = field.ty();

                if field.is_many() {
                    quote! {
                        #[inline]
                        pub fn #method_name(&self) -> AstChildren<#ty> {
                            support::children(&self.syntax)
                        }
                    }
                } else if let Some(token_kind) = field.token_kind() {
                    quote! {
                        #[inline]
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::token(&self.syntax, #token_kind)
                        }
                    }
                } else {
                    quote! {
                        #[inline]
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::child(&self.syntax)
                        }
                    }
                }
            });
            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }

                    #(#traits)*

                    impl #name {
                        #(#methods)*
                    }
                },
                quote! {
                    impl AstNode for #name {
                        #[inline]
                        fn can_cast(kind: SyntaxKind) -> bool {
                            kind == #kind
                        }
                        #[inline]
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                        }
                        #[inline]
                        fn syntax(&self) -> &SyntaxNode { &self.syntax }
                    }

                    impl hash::Hash for #name {
                        fn hash<H: hash::Hasher>(&self, state: &mut H) {
                            self.syntax.hash(state);
                        }
                    }

                    impl Eq for #name {}
                    impl PartialEq for #name {
                        fn eq(&self, other: &Self) -> bool {
                            self.syntax == other.syntax
                        }
                    }

                    impl Clone for #name {
                        fn clone(&self) -> Self {
                            Self { syntax: self.syntax.clone() }
                        }
                    }

                    impl fmt::Debug for #name {
                        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                            f.debug_struct(#node_str_name).field("syntax", &self.syntax).finish()
                        }
                    }
                },
            )
        })
        .unzip();

    let (enum_defs, enum_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .enums
        .iter()
        .map(|en| {
            let (mut enum_variants, mut variants): (Vec<_>, Vec<_>) =
                en.variants.iter().partition_map(|variant| {
                    match grammar.enums.iter().find(|enm| enm.name == *variant) {
                        Some(enm) => itertools::Either::Left(format_ident!("{}", enm.name)),
                        None => itertools::Either::Right(format_ident!("{}", variant)),
                    }
                });
            enum_variants.sort();
            variants.sort();
            let name = format_ident!("{}", en.name);
            let kinds: Vec<_> = variants
                .iter()
                .map(|name| format_ident!("{}", to_upper_snake_case(&name.to_string())))
                .collect();
            let can_cast_enum_arms = enum_variants.iter().map(|variant| {
                let variant = format_ident!("{}", variant);
                quote!(_ if #variant::can_cast(kind) => true)
            });
            let traits = en.traits.iter().sorted().map(|trait_name| {
                let trait_name = format_ident!("{}", trait_name);
                quote!(impl ast::#trait_name for #name {})
            });

            let ast_node = quote! {
                impl AstNode for #name {
                    #[inline]
                    fn can_cast(kind: SyntaxKind) -> bool {
                        #[allow(clippy::match_like_matches_macro)]
                        match kind {
                            #(#kinds)|* => true,
                            #(#can_cast_enum_arms,)*
                            _ => false,
                        }
                    }
                    #[inline]
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        match syntax.kind() {
                            #(
                            #kinds => Some(#name::#variants(#variants { syntax })),
                            )*
                            _ => {
                                #(
                                    if #enum_variants::can_cast(syntax.kind()) {
                                        return Some(#name::#enum_variants(#enum_variants::cast(syntax).expect("cast failed")));
                                    }
                                )*
                                None
                            }
                        }
                    }
                    #[inline]
                    fn syntax(&self) -> &SyntaxNode {
                        match self {
                            #(
                            #name::#variants(it) => &it.syntax,
                            )*
                            #(
                            #name::#enum_variants(it) => it.syntax(),
                            )*
                        }
                    }
                }
            };

            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub enum #name {
                        #(#variants(#variants),)*
                        #(#enum_variants(#enum_variants),)*
                    }

                    #(#traits)*
                },
                quote! {
                    #(
                        impl From<#variants> for #name {
                            #[inline]
                            fn from(node: #variants) -> #name {
                                #name::#variants(node)
                            }
                        }
                    )*
                    #(
                        impl From<#enum_variants> for #name {
                            #[inline]
                            fn from(node: #enum_variants) -> #name {
                                #name::#enum_variants(node)
                            }
                        }
                    )*
                    #ast_node
                },
            )
        })
        .unzip();
    let (any_node_defs, any_node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .flat_map(|node| node.traits.iter().map(move |t| (t, node)))
        .into_group_map()
        .into_iter()
        .sorted_by_key(|(name, _)| *name)
        .map(|(trait_name, nodes)| {
            let name = format_ident!("Any{}", trait_name);
            let node_str_name = name.to_string();
            let trait_name = format_ident!("{}", trait_name);
            let kinds: Vec<_> = nodes
                .iter()
                .map(|name| format_ident!("{}", to_upper_snake_case(&name.name.to_string())))
                .collect();
            let nodes = nodes.iter().map(|node| format_ident!("{}", node.name));
            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }
                    impl #name {
                        #[inline]
                        pub fn new<T: ast::#trait_name>(node: T) -> #name {
                            #name {
                                syntax: node.syntax().clone()
                            }
                        }
                    }

                },
                quote! {
                    impl ast::#trait_name for #name {}
                    impl AstNode for #name {
                        #[inline]
                        fn can_cast(kind: SyntaxKind) -> bool {
                            matches!(kind, #(#kinds)|*)
                        }
                        #[inline]
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            Self::can_cast(syntax.kind()).then_some(#name { syntax })
                        }
                        #[inline]
                        fn syntax(&self) -> &SyntaxNode {
                            &self.syntax
                        }
                    }

                    impl hash::Hash for #name {
                        fn hash<H: hash::Hasher>(&self, state: &mut H) {
                            self.syntax.hash(state);
                        }
                    }

                    impl Eq for #name {}
                    impl PartialEq for #name {
                        fn eq(&self, other: &Self) -> bool {
                            self.syntax == other.syntax
                        }
                    }

                    impl Clone for #name {
                        fn clone(&self) -> Self {
                            Self { syntax: self.syntax.clone() }
                        }
                    }

                    impl fmt::Debug for #name {
                        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                            f.debug_struct(#node_str_name).field("syntax", &self.syntax).finish()
                        }
                    }

                    #(
                        impl From<#nodes> for #name {
                            #[inline]
                            fn from(node: #nodes) -> #name {
                                #name { syntax: node.syntax }
                            }
                        }
                    )*
                },
            )
        })
        .unzip();

    let enum_names = grammar.enums.iter().map(|it| &it.name);
    let node_names = grammar.nodes.iter().map(|it| &it.name);

    let display_impls =
        enum_names.chain(node_names.clone()).map(|it| format_ident!("{}", it)).map(|name| {
            quote! {
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            }
        });

    let defined_nodes: HashSet<_> = node_names.collect();
    kinds
        .nodes
        .iter()
        .map(|kind| to_pascal_case(kind))
        .filter(|name| !defined_nodes.iter().any(|&it| it == name))
        .for_each(|node| panic!("Error: node {node} is defined in the grammar (vip.ungram) but no AST node was generated for it"));

    let ast = quote! {
        use std::{fmt, hash};

        use crate::{
            SyntaxNode, SyntaxToken, SyntaxKind::{self, *},
            ast::{self, AstNode, AstChildren, support},
            T,
        };

        #(#node_defs)*
        #(#enum_defs)*
        #(#any_node_defs)*
        #(#node_boilerplate_impls)*
        #(#enum_boilerplate_impls)*
        #(#any_node_boilerplate_impls)*
        #(#display_impls)*
    };

    let ast = ast.to_string().replace("T ! [", "T![");

    let mut res = String::with_capacity(ast.len() * 2);

    let mut docs =
        grammar.nodes.iter().map(|it| &it.doc).chain(grammar.enums.iter().map(|it| &it.doc));

    for chunk in ast.split("# [pretty_doc_comment_placeholder_workaround] ") {
        res.push_str(chunk);
        if let Some(doc) = docs.next() {
            write_doc_comment(doc, &mut res);
        }
    }

    let res = add_preamble(crate::flags::CodegenType::Grammar, reformat(res));
    res.replace("#[derive", "\n#[derive")
}

fn write_doc_comment(contents: &[String], dest: &mut String) {
    for line in contents {
        writeln!(dest, "///{line}").unwrap();
    }
}

fn generate_syntax_kinds(grammar: KindsSrc) -> String {
    let (single_char_tokens_values, single_char_tokens): (Vec<_>, Vec<_>) = grammar
        .punct
        .iter()
        .filter(|(token, _name)| token.chars().count() == 1)
        .map(|(token, name)| (token.chars().next().unwrap(), format_ident!("{}", name)))
        .unzip();

    let punctuation_values = grammar.punct.iter().map(|(token, _name)| {
        if "{}[]()\\".contains(token) {
            let c = token.chars().next().unwrap();
            quote! { #c }
        } else {
            let cs = token.chars().map(|c| Punct::new(c, Spacing::Joint));
            quote! { #(#cs)* }
        }
    });

    let punctuation =
        grammar.punct.iter().map(|(_token, name)| format_ident!("{}", name)).collect::<Vec<_>>();
    let punctuation_texts = grammar.punct.iter().map(|&(text, _name)| text);

    let to_keyword = |name: &&str| format_ident!("{}_KW", to_upper_snake_case(name));

    let strict_keywords = grammar.keywords;
    let strict_keywords_variants = strict_keywords.iter().map(to_keyword).collect::<Vec<_>>();
    let strict_keywords_tokens = strict_keywords.iter().map(|it| format_ident!("{}", it));

    let contextual_keywords = grammar.contextual_keywords;
    let contextual_keywords_variants =
        contextual_keywords.iter().map(to_keyword).collect::<Vec<_>>();
    let contextual_keywords_tokens = contextual_keywords.iter().map(|it| format_ident!("{}", it));

    let hash_punct = Punct::new('#', Spacing::Joint);
    let hashtag = quote! { #hash_punct };
    let directives = grammar.directives.iter().collect::<Vec<_>>();
    let (directives_tokens, directives_variants): (Vec<_>, Vec<_>) = grammar
        .directives
        .iter()
        .map(|kw| match kw.strip_prefix('#') {
            Some(kw) => {
                let ident = format_ident!("{}", kw);
                (quote! { #hashtag #ident }, format_ident!("HASH{}", to_upper_snake_case(kw)))
            }
            None => panic!("Directive {kw} must start with #"),
        })
        .unzip();

    let literals =
        grammar.literals.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

    let tokens = grammar.tokens.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

    let nodes = grammar.nodes.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

    let ast = quote! {
        #![expect(non_camel_case_types)]
        /// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
        #[derive(Debug)]
        #[repr(u16)]
        pub enum SyntaxKind {
            // Technical SyntaxKinds: they appear temporally during parsing,
            // but never end up in the final tree
            #[doc(hidden)]
            TOMBSTONE,
            #[doc(hidden)]
            EOF,
            #(#punctuation,)*
            #(#strict_keywords_variants,)*
            #(#contextual_keywords_variants,)*
            #(#directives_variants,)*
            #(#literals,)*
            #(#tokens,)*
            #[doc(hidden)]
            __LAST_TOKEN,
            #(#nodes,)*

            // Technical kind so that we can cast from u16 safely
            #[doc(hidden)]
            __LAST,
        }
        use self::SyntaxKind::*;

        impl SyntaxKind {
            pub const fn text(self) -> &'static str {
                match self {
                    TOMBSTONE | EOF | __LAST | __LAST_TOKEN
                    #( | #literals )*
                    #( | #nodes )*
                    #( | #tokens )* => panic!("no text for these `SyntaxKind`s"),
                    #( #punctuation => #punctuation_texts ,)*
                    #( #strict_keywords_variants => #strict_keywords ,)*
                    #( #contextual_keywords_variants => #contextual_keywords ,)*
                    #( #directives_variants => #directives ,)*
                }
            }

            /// Checks whether this syntax kind is a strict or weak keyword.
            pub fn is_strict_keyword(self) -> bool {
                matches!(self, #(#strict_keywords_variants)|*)
            }

            /// Checks whether this syntax kind is a contextual keyword.
            /// Contextual keywords are identifiers that are considered keywords only in certain contexts.
            pub fn is_contextual_keyword(self) -> bool {
                matches!(self, #(#contextual_keywords_variants)|*)
            }

            /// Checks whether this syntax kind is a keyword (both strict and contextual).
            pub fn is_keyword(self) -> bool {
                self.is_strict_keyword() || self.is_contextual_keyword()
            }

            pub fn is_directive(self) -> bool {
                matches!(self, #(#directives_variants)|*)
            }

            pub fn is_punct(self) -> bool {
                matches!(self, #(#punctuation)|*)
            }

            /// Try to build a strict keyword from a string.
            pub fn from_strict_keyword(ident: &str) -> Option<SyntaxKind> {
                let kw = match ident {
                    #(#strict_keywords => #strict_keywords_variants,)*
                    _ => return None,
                };
                Some(kw)
            }

            /// Try to build a contextual keyword from a string.
            pub fn from_contextual_keyword(ident: &str) -> Option<SyntaxKind> {
                let kw = match ident {
                    #(#contextual_keywords => #contextual_keywords_variants,)*
                    _ => return None,
                };
                Some(kw)
            }

            /// Try to build a keyword (both strict and contextual) from a string.
            pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
                Self::from_strict_keyword(ident).or_else(|| Self::from_contextual_keyword(ident))
            }

            pub fn from_directive(ident: &str) -> Option<SyntaxKind> {
                let kw = match ident {
                    #(#directives => #directives_variants,)*
                    _ => return None,
                };
                Some(kw)
            }

            pub fn from_char(c: char) -> Option<SyntaxKind> {
                let tok = match c {
                    #(#single_char_tokens_values => #single_char_tokens,)*
                    _ => return None,
                };
                Some(tok)
            }
        }

        #[macro_export]
        macro_rules! T_ {
            #([#punctuation_values] => { $crate::SyntaxKind::#punctuation };)*
            #([#strict_keywords_tokens] => { $crate::SyntaxKind::#strict_keywords_variants };)*
            #([#contextual_keywords_tokens] => { $crate::SyntaxKind::#contextual_keywords_variants };)*
            #([#directives_tokens] => { $crate::SyntaxKind::#directives_variants };)*
            [ident] => { $crate::SyntaxKind::IDENT };
            [var] => { $crate::SyntaxKind::VAR };
            [string] => { $crate::SyntaxKind::STRING };
            [int_number] => { $crate::SyntaxKind::INT_NUMBER };
            [float_number] => { $crate::SyntaxKind::FLOAT_NUMBER };
        }

        impl ::core::marker::Copy for SyntaxKind {}
        impl ::core::clone::Clone for SyntaxKind {
            #[inline]
            fn clone(&self) -> Self {
                *self
            }
        }
        impl ::core::cmp::PartialEq for SyntaxKind {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                (*self as u16) == (*other as u16)
            }
        }
        impl ::core::cmp::Eq for SyntaxKind {}
        impl ::core::cmp::PartialOrd for SyntaxKind {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> core::option::Option<core::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        impl ::core::cmp::Ord for SyntaxKind {
            #[inline]
            fn cmp(&self, other: &Self) -> core::cmp::Ordering {
                (*self as u16).cmp(&(*other as u16))
            }
        }
        impl ::core::hash::Hash for SyntaxKind {
            fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
                ::core::mem::discriminant(self).hash(state);
            }
        }
    };

    add_preamble(crate::flags::CodegenType::Grammar, reformat(ast.to_string()))
}

fn to_upper_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        buf.push(c.to_ascii_uppercase());
    }
    buf
}

fn to_lower_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        buf.push(c.to_ascii_lowercase());
    }
    buf
}

fn to_pascal_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev_is_underscore = true;
    for c in s.chars() {
        if c == '_' {
            prev_is_underscore = true;
        } else if prev_is_underscore {
            buf.push(c.to_ascii_uppercase());
            prev_is_underscore = false;
        } else {
            buf.push(c.to_ascii_lowercase());
        }
    }
    buf
}

fn pluralize(s: &str) -> String {
    if s[s.len() - 2..] == *"ty" {
        let f = &s[..s.len() - 2];
        format!("{}ties", f)
    } else {
        format!("{s}s")
    }
}

impl Field {
    fn is_many(&self) -> bool {
        matches!(self, Field::Node { cardinality: Cardinality::Many, .. })
    }
    fn token_kind(&self) -> Option<proc_macro2::TokenStream> {
        match self {
            Field::Token(token) => {
                let token: proc_macro2::TokenStream = token.parse().unwrap();
                Some(quote! { T![#token] })
            }
            _ => None,
        }
    }
    fn method_name(&self) -> String {
        match self {
            Field::Token(name) => {
                let name = match name.as_str() {
                    ";" => "semicolon",
                    "->" => "arrow",
                    "=>" => "fat_arrow",
                    "'{'" => "l_curly",
                    "'}'" => "r_curly",
                    "'('" => "l_paren",
                    "')'" => "r_paren",
                    "'['" => "l_brack",
                    "']'" => "r_brack",
                    "'\\\\'" => "backslash",
                    "<" => "lt",
                    ">" => "qt",
                    "=" => "eq",
                    "!" => "excl",
                    "*" => "star",
                    "**" => "starstar",
                    "&" => "amp",
                    "-" => "minus",
                    "_" => "underscore",
                    "." => "dot",
                    ".." => "dotdot",
                    "..." => "dotdotdot",
                    "@" => "at",
                    ":" => "colon",
                    "::" => "coloncolon",
                    "#" => "hashtag",
                    "?" => "question_mark",
                    "," => "comma",
                    "|" => "pipe",
                    "||" => "pipepipe",
                    "~" => "tilde",
                    "~~" => "tildetilde",
                    "/" => "slash",
                    ":-" => "colondash",
                    "$" => "dollar",
                    ":=" => "coloneq",
                    "+" => "plus",
                    name if name.starts_with('#') => {
                        let name = &name[1..];
                        return format!("hash{name}_token");
                    }
                    _ => name,
                };
                format!("{name}_token",)
            }
            Field::Node { name, .. } => {
                if name == "type" {
                    String::from("ty")
                } else {
                    name.to_owned()
                }
            }
        }
    }
    fn ty(&self) -> proc_macro2::Ident {
        match self {
            Field::Token(_) => format_ident!("SyntaxToken"),
            Field::Node { ty, .. } => format_ident!("{}", ty),
        }
    }
}

fn clean_token_name(name: &str) -> String {
    let cleaned = name.trim_start_matches(['@', '&', '?']);
    if cleaned.is_empty() { name.to_owned() } else { cleaned.to_owned() }
}

fn lower(grammar: &Grammar) -> AstSrc {
    let mut res = AstSrc {
        tokens: "Whitespace Comment String IntNumber FloatNumber Ident Var"
            .split_ascii_whitespace()
            .map(|it| it.to_owned())
            .collect::<Vec<_>>(),
        ..Default::default()
    };

    for &node in &grammar.iter().collect::<Vec<_>>() {
        let name = grammar[node].name.clone();
        let rule = &grammar[node].rule;
        let _g = panic_context::enter(name.clone());
        match lower_enum(grammar, rule) {
            Some(variants) => {
                let enum_src = AstEnumSrc { doc: Vec::new(), name, traits: Vec::new(), variants };
                res.enums.push(enum_src);
            }
            None => {
                let mut fields = Vec::new();
                lower_rule(&mut fields, grammar, None, rule);
                res.nodes.push(AstNodeSrc { doc: Vec::new(), name, traits: Vec::new(), fields });
            }
        }
    }

    deduplicate_fields(&mut res);
    extract_enums(&mut res);
    extract_struct_traits(&mut res);
    extract_enum_traits(&mut res);
    res.nodes.sort_by_key(|it| it.name.clone());
    res.enums.sort_by_key(|it| it.name.clone());
    res.tokens.sort();
    res.nodes.iter_mut().for_each(|it| {
        it.traits.sort();
        it.fields.sort_by_key(|it| match it {
            Field::Token(name) => (true, name.clone()),
            Field::Node { name, .. } => (false, name.clone()),
        });
    });
    res.enums.iter_mut().for_each(|it| {
        it.traits.sort();
        it.variants.sort();
    });
    res
}

fn lower_enum(grammar: &Grammar, rule: &Rule) -> Option<Vec<String>> {
    let alternatives = match rule {
        Rule::Alt(it) => it,
        _ => return None,
    };
    let mut variants = Vec::new();
    for alternative in alternatives {
        match alternative {
            Rule::Node(it) => variants.push(grammar[*it].name.clone()),
            _ => return None,
        }
    }
    Some(variants)
}

fn lower_rule(acc: &mut Vec<Field>, grammar: &Grammar, label: Option<&String>, rule: &Rule) {
    if lower_separated_list(acc, grammar, label, rule) {
        return;
    }

    match rule {
        Rule::Node(node) => {
            let ty = grammar[*node].name.clone();
            let name = label.cloned().unwrap_or_else(|| to_lower_snake_case(&ty));
            let field = Field::Node { name, ty, cardinality: Cardinality::Optional };
            acc.push(field);
        }
        Rule::Token(token) => {
            assert!(label.is_none());
            let mut name = clean_token_name(&grammar[*token].name);
            if "[]{}()".contains(&name) {
                name = format!("'{name}'");
            } else if name == "\\" {
                "'\\\\'".clone_into(&mut name)
            }
            let field = Field::Token(name);
            acc.push(field);
        }
        Rule::Rep(inner) => {
            if let Rule::Node(node) = &**inner {
                let ty = grammar[*node].name.clone();
                let name = label.cloned().unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
                let field = Field::Node { name, ty, cardinality: Cardinality::Many };
                acc.push(field);
                return;
            }
            panic!("unhandled rule: {rule:?}");
        }
        Rule::Labeled { label: l, rule } => {
            assert!(label.is_none());

            // Labels mentioned here are will not be generated as functions for the corresponding ast node.
            // This is useful for methods which for some reason should be implemented manually.
            let labels = ["lhs", "rhs", "op"];
            if labels.contains(&l.as_str()) {
                return;
            }

            lower_rule(acc, grammar, Some(l), rule);
        }
        Rule::Seq(rules) | Rule::Alt(rules) => {
            for rule in rules {
                lower_rule(acc, grammar, label, rule)
            }
        }
        Rule::Opt(rule) => lower_rule(acc, grammar, label, rule),
    }
}

// (T (',' T)* ','?)
fn lower_separated_list(
    acc: &mut Vec<Field>,
    grammar: &Grammar,
    label: Option<&String>,
    rule: &Rule,
) -> bool {
    let sequence = match rule {
        Rule::Seq(it) => it,
        _ => return false,
    };
    let (nt, repeat, trailing_sep) = match sequence.as_slice() {
        [Rule::Node(node), Rule::Rep(repeat), Rule::Opt(trailing_sep)] => {
            (Either::Left(node), repeat, Some(trailing_sep))
        }
        [Rule::Node(node), Rule::Rep(repeat)] => (Either::Left(node), repeat, None),
        [Rule::Token(token), Rule::Rep(repeat), Rule::Opt(trailing_sep)] => {
            (Either::Right(token), repeat, Some(trailing_sep))
        }
        [Rule::Token(token), Rule::Rep(repeat)] => (Either::Right(token), repeat, None),
        _ => return false,
    };
    let repeat_seq = match &**repeat {
        Rule::Seq(repeat_seq) => repeat_seq,
        _ => return false,
    };
    if !matches!(
        repeat_seq.as_slice(),
        [first_sep, nt_]
            if trailing_sep.is_none_or(|sep| first_sep == &**sep) && match (nt, nt_) {
                (Either::Left(node), Rule::Node(nt_)) => node == nt_,
                (Either::Right(token), Rule::Token(nt_)) => token == nt_,
                _ => false,
            }
    ) {
        return false;
    }
    match nt {
        Either::Right(token) => {
            let name = clean_token_name(&grammar[*token].name);
            let field = Field::Token(name);
            acc.push(field);
        }
        Either::Left(node) => {
            let ty = grammar[*node].name.clone();
            let name = label.cloned().unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
            let field = Field::Node { name, ty, cardinality: Cardinality::Many };
            acc.push(field);
        }
    }
    true
}

fn deduplicate_fields(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        let mut i = 0;
        while i < node.fields.len() {
            for j in 0..i {
                let f1 = &node.fields[i];
                let f2 = &node.fields[j];
                if f1 == f2 {
                    panic!(
                        "node {:?} has duplicated fields for {:?} {:?}. Disambiguate them!",
                        node.name,
                        f1.ty(),
                        f1.method_name()
                    );
                }
            }
            i += 1;
        }
    }
}

fn extract_enums(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        for alternation in &ast.enums {
            let mut to_remove = Vec::new();
            for (i, field) in node.fields.iter().enumerate() {
                let ty = field.ty().to_string();
                if alternation.variants.iter().any(|it| it == &ty) {
                    to_remove.push(i);
                }
            }
            if to_remove.len() == alternation.variants.len() {
                node.remove_field(to_remove);
                let ty = alternation.name.clone();
                let name = to_lower_snake_case(&ty);
                node.fields.push(Field::Node { name, ty, cardinality: Cardinality::Optional });
            }
        }
    }
}

const TRAIT_SUBS: &[(&str, &[&str])] = &[
    // (trait name, methods which trait replaces)
    ("HasItems", &["items"]),
    ("HasStringSeq", &["string_seq"]),
    ("HasScopeNameDecl", &["scope_name_decl"]),
    ("HasImplementSections", &["implement_sections"]),
    ("HasClassSections", &["class_sections"]),
    ("HasInterfaceSections", &["interface_sections"]),
    ("HasAttributes", &["attribute_list"]),
    ("HasFormalParamList", &["formal_param_list"]),
    ("HasPredParams", &["pred_params"]),
    ("HasType", &["ty"]),
];

fn extract_struct_traits(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        for (trait_name, functions) in TRAIT_SUBS {
            extract_struct_trait(node, trait_name, functions);
        }
    }
}

fn extract_struct_trait(node: &mut AstNodeSrc, trait_name: &str, methods: &[&str]) {
    let mut to_remove = Vec::new();
    for (i, field) in node.fields.iter().enumerate() {
        let method_name = field.method_name();
        if methods.iter().any(|&it| it == method_name) {
            to_remove.push(i);
        }
    }
    if to_remove.len() == methods.len() {
        node.traits.push(trait_name.to_owned());
        node.remove_field(to_remove);
    }
}

fn extract_enum_traits(ast: &mut AstSrc) {
    let enums = &ast.enums.clone();
    for enm in &mut ast.enums {
        let nodes = &ast.nodes;
        let mut variant_traits = enm
            .variants
            .iter()
            .map(|variant| {
                nodes
                    .iter()
                    .find(|it| it.name == *variant)
                    .map(|n| n.traits.clone())
                    .or_else(|| {
                        enums.iter().find(|it| it.name == *variant).map(|n| n.traits.clone())
                    })
                    .unwrap()
            })
            .map(|traits| traits.iter().cloned().collect::<BTreeSet<_>>());

        let mut enum_traits = match variant_traits.next() {
            Some(it) => it,
            None => continue,
        };
        for traits in variant_traits {
            enum_traits = enum_traits.intersection(&traits).cloned().collect();
        }
        enm.traits = enum_traits.into_iter().collect();
    }
}

impl AstNodeSrc {
    fn remove_field(&mut self, to_remove: Vec<usize>) {
        to_remove.into_iter().rev().for_each(|idx| {
            self.fields.remove(idx);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        generate(true);
    }
}
