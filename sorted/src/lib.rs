extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::quote;
use quote::ToTokens;
use syn::parse_macro_input;
use syn::visit_mut::VisitMut;
use syn::{spanned::Spanned, Arm, Error, ExprMatch, Ident, Item, ItemFn, Pat, Path, Result};

fn check_enum(ast: &Item) -> Result<()> {
    match ast {
        Item::Enum(enum_ast) => {
            let mut prev = None;
            for v in enum_ast.variants.iter() {
                if let Some(ident_prev) = prev {
                    if &v.ident < ident_prev {
                        // re-iter the variants to find the proper location
                        for var in enum_ast.variants.iter() {
                            if var.ident > v.ident {
                                let error = Error::new(
                                    v.ident.span(),
                                    format!("{} should sort before {}", v.ident, var.ident),
                                );
                                return Err(error);
                            }
                        }
                    }
                }
                prev = Some(&v.ident);
            }
            Ok(())
        }
        _ => Err(Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn check_pat_match<'a, Iter: Iterator<Item = &'a Pat> + Clone>(pat_iter: Iter) -> Result<()> {
    let mut path_prev = None;
    let mut ident_prev = None;
    for pat in pat_iter.clone() {
        if let Some(prev) = path_prev {
            let cur_path = check_and_get_pat_path(pat)?;
            if gt(prev, cur_path) {
                for p in pat_iter.clone() {
                    let path = check_and_get_pat_path(p)?;
                    if gt(path, cur_path) {
                        return Err(Error::new(
                            cur_path.span(),
                            format!(
                                "{} should sort before {}",
                                fmt_path(cur_path),
                                fmt_path(path)
                            ),
                        ));
                    }
                }
            }
            path_prev = Some(cur_path);
            ident_prev = None;
        } else if let Some(prev) = ident_prev {
            let ident = check_and_get_pat_ident(pat)?;
            if prev > ident {
                return Err(Error::new(
                    ident.span(),
                    format!("{} should sort before {}", ident, prev),
                ));
            }

            ident_prev = Some(ident);
            path_prev = None;
        } else {
            assert!(ident_prev.is_none() && path_prev.is_none());
            if let Pat::Ident(i) = pat {
                ident_prev = Some(&i.ident);
            } else {
                let path = check_and_get_pat_path(pat)?;
                match path.get_ident() {
                    Some(i) => ident_prev = Some(i),
                    None => path_prev = Some(path),
                }
            }
        }
    }
    Ok(())
}

fn fmt_path(p: &Path) -> String {
    p.to_token_stream().to_string().replace(" ", "")
}

fn gt(p1: &Path, p2: &Path) -> bool {
    for (s1, s2) in p1.segments.iter().zip(p2.segments.iter()) {
        if s1.ident > s2.ident {
            return true;
        }
    }
    p1.segments.len() > p2.segments.len()
}

fn check_and_get_pat_path(pat: &Pat) -> Result<&Path> {
    match pat {
        Pat::Path(p) => Ok(&p.path),
        Pat::Or(o) => {
            check_pat_match(o.cases.iter())?;
            check_and_get_pat_path(o.cases.first().unwrap())
        }
        Pat::Reference(r) => check_and_get_pat_path(r.pat.as_ref()),
        Pat::Struct(s) => Ok(&s.path),
        Pat::TupleStruct(t) => Ok(&t.path),
        pat => Err(Error::new(
            pat.span().unwrap().into(),
            "unsupported by #[sorted]",
        )),
    }
}

fn check_and_get_pat_ident(pat: &Pat) -> Result<&Ident> {
    match pat {
        Pat::Ident(i) => Ok(&i.ident),
        p => {
            let path = check_and_get_pat_path(p)?;
            match path.get_ident() {
                Some(i) => Ok(i),
                None => unimplemented!(),
            }
        }
    }
}

fn check_pat(mut arms: &[Arm]) -> Result<()> {
    if !arms.is_empty() {
        let len = arms.len();
        if let Pat::Wild(_) = arms[len - 1].pat {
            arms = &arms[..len - 1];
        }
        check_pat_match(arms.iter().map(|a| &a.pat))
    } else {
        Ok(())
    }
}

struct CheckVisitor {
    result: Result<()>,
}

impl CheckVisitor {
    fn new() -> Self {
        CheckVisitor { result: Ok(()) }
    }
}

impl VisitMut for CheckVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        let check_idx = i
            .attrs
            .iter()
            .enumerate()
            .filter_map(|(idx, attr)| {
                if attr.path.segments.len() == 1
                    && attr.path.segments.first().unwrap().ident == "sorted"
                {
                    Some(idx)
                } else {
                    None
                }
            })
            .next();

        if let Some(idx) = check_idx {
            i.attrs.remove(idx);
            self.result = check_pat(&i.arms);
        }
    }
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let ast = parse_macro_input!(input as Item);

    let error = match check_enum(&ast) {
        Err(e) => Some(e.to_compile_error()),
        _ => None,
    };

    (quote! {
        #ast
        #error
    })
    .into()
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut ast = parse_macro_input!(input as ItemFn);

    let mut visitor = CheckVisitor::new();
    visitor.visit_item_fn_mut(&mut ast);

    let error = match visitor.result {
        Err(e) => Some(e.to_compile_error()),
        _ => None,
    };

    (quote! {
        #ast
        #error
    })
    .into()
}
