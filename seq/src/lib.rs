extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2;

use proc_macro2::{Group, Ident, Literal, TokenTree};
use syn::parse::{Parse, ParseBuffer};
use syn::{parse_macro_input, LitInt, Token};

use std::iter::FromIterator;

struct SeqProc {
    var: Ident,
    _in_ident: Token![in],
    start: LitInt,
    _range_puncs: Token![..],
    inclusive: Option<Token![=]>,
    end: LitInt,
    body: proc_macro2::Group,
}

impl Parse for SeqProc {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        let var: Ident = input.parse()?;
        let in_ident: Token![in] = input.parse()?;
        let start: LitInt = input.parse()?;
        let range_puncs: Token![..] = input.parse()?;
        let inclusive: Option<Token![=]> = if let Ok(i) = input.parse() {
            Some(i)
        } else {
            None
        };
        let end: LitInt = input.parse()?;
        let body: Group = input.parse()?;
        Ok(Self {
            var,
            _in_ident: in_ident,
            start,
            _range_puncs: range_puncs,
            inclusive,
            end,
            body,
        })
    }
}

impl SeqProc {
    fn range(&self) -> impl Iterator<Item = i64> {
        let start: i64 = self.start.base10_parse().unwrap();
        let mut end: i64 = self.end.base10_parse().unwrap();
        // FIXME: if end is `i64::max_value()` this may overflow
        if self.inclusive.is_some() {
            end += 1;
        }
        start..end
    }

    fn build(&self) -> TokenStream {
        let token_trees: Vec<_> = self.body.stream().into_iter().collect();
        let mut res = vec![];
        let mut ended = false;
        for cur_seq in self.range() {
            self.process_token_trees(&token_trees, cur_seq, &mut res, &mut ended);
            if ended {
                break;
            }
        }

        (proc_macro2::TokenStream::from_iter(res)).into()
    }

    fn build_token_group(&self, group: &Group, seq: i64, should_end: &mut bool) -> TokenTree {
        let mut res = vec![];
        self.process_token_group(group, seq, &mut res, should_end);
        TokenTree::Group(Group::new(
            group.delimiter(),
            proc_macro2::TokenStream::from_iter(res),
        ))
    }

    fn process_token_group(
        &self,
        group: &Group,
        seq: i64,
        res: &mut Vec<TokenTree>,
        should_end: &mut bool,
    ) {
        let token_trees: Vec<_> = group.stream().into_iter().collect();
        self.process_token_trees(&token_trees, seq, res, should_end);
    }

    fn process_token_trees(
        &self,
        token_trees: &[TokenTree],
        seq: i64,
        res: &mut Vec<TokenTree>,
        should_end: &mut bool,
    ) {
        let len = token_trees.len();
        let mut i = 0;
        while i < len {
            match &token_trees[i] {
                TokenTree::Group(group) => {
                    let new_tree = self.build_token_group(group, seq, should_end);
                    res.push(new_tree);
                }
                TokenTree::Ident(ident) => {
                    let mut add = true;
                    if ident.eq(&self.var) {
                        let t = TokenTree::Literal(Literal::i64_unsuffixed(seq));
                        res.push(t);
                        add = false;
                    } else if i < len - 2 {
                        if let TokenTree::Punct(ref p) = token_trees[i + 1] {
                            if let TokenTree::Ident(ref v) = token_trees[i + 2] {
                                if p.as_char() == '#' && v.eq(&self.var) {
                                    let ident_value = format!("{}{}", ident, seq);
                                    res.push(TokenTree::Ident(Ident::new(
                                        &ident_value,
                                        ident.span(),
                                    )));
                                    i += 2;
                                    add = false;
                                }
                            }
                        }
                    }
                    if add {
                        res.push(TokenTree::Ident(ident.clone()));
                    }
                }
                TokenTree::Punct(p) => {
                    let mut add = true;
                    if p.as_char() == '#' && i < len - 1 {
                        if let TokenTree::Ident(ref ident) = token_trees[i + 1] {
                            if ident == &self.var {
                                let t = TokenTree::Literal(Literal::i64_unsuffixed(seq));
                                res.push(t);
                                i += 1;
                                add = false;
                            }
                        } else if i < len - 2 {
                            if let TokenTree::Group(ref g) = token_trees[i + 1] {
                                if let TokenTree::Punct(p) = &token_trees[i + 2] {
                                    if p.as_char() == '*' {
                                        // match a repeat pattern, should resolve this group repeatedly
                                        for s in self.range() {
                                            self.process_token_group(g, s, res, should_end);
                                        }
                                        i += 2;
                                        *should_end = true;
                                        add = false;
                                    }
                                }
                            }
                        }
                    }
                    if add {
                        res.push(TokenTree::Punct(p.clone()));
                    }
                }
                TokenTree::Literal(l) => {
                    res.push(TokenTree::Literal(l.clone()));
                }
            }
            i += 1;
        }
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq_input = parse_macro_input!(input as SeqProc);

    seq_input.build()
}
