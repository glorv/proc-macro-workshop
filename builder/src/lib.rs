extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput};

macro_rules! extract {
    ($name:ident, $body: expr) => {
        fn $name<'a>(data: &'a syn::DataStruct) -> impl Iterator<Item = TokenStream> + 'a {
            data.fields.iter().map($body)
        }
    };
}

extract!(fields, |f| {
    let ident = f.ident.as_ref().unwrap();
    let ty = &f.ty;
    if field_is(f, "Option") || field_is(f, "Vec") {
        quote! {
            #ident: #ty,
        }
    } else {
        quote! {
            #ident: Option<#ty>,
        }
    }
});

extract!(init_fields, |f| {
    let ident = &f.ident;
    if field_is(f, "Vec") {
        quote! {
            #ident: vec![],
        }
    } else {
        quote! {
            #ident: None,
        }
    }
});

extract!(setters, |f| {
    let n = &f.ident;
    let ty = &f.ty;

    if field_is(f, "Vec") {
        let mut setter = String::new();
        for meta_items in f.attrs.iter().filter_map(get_field_meta_items) {
            for item in meta_items {
                match item {
                    syn::NestedMeta::Meta(syn::Meta::NameValue(ref m))
                        if m.path.is_ident("each") =>
                    {
                        if let syn::Lit::Str(s) = &m.lit {
                            setter = s.value();
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }

        if !setter.is_empty() {
            let ident = syn::Ident::new(&setter, n.span());
            let ty = get_inner_generic_type(f);
            quote! {
                fn #ident(&mut self, #n: #ty) -> &mut Self {
                    self.#n.push(#n);
                    self
                }
            }
        } else {
            quote! {
                fn #n(&mut self, #n: #ty) -> &mut Self {
                    self.#n = #n;
                    self
                }
            }
        }
    } else if field_is(f, "Option") {
        let ty = get_inner_generic_type(f);
        quote! {
            fn #n(&mut self, #n: #ty) -> &mut Self {
                self.#n = Some(#n);
                self
            }
        }
    } else {
        quote! {
            fn #n(&mut self, #n: #ty) -> &mut Self {
                self.#n = Some(#n);
                self
            }
        }
    }
});

extract!(check_fields, |f| {
    let ident = &f.ident;
    if !field_is(f, "Option") && !field_is(f, "Vec") {
        let error_msg = format!("Error: {} is None", ident.as_ref().unwrap());
        quote! {
            if self.#ident.is_none() {
                return Err(<Box<dyn std::error::Error>>::from(String::from(#error_msg)));
            }
        }
    } else {
        quote!()
    }
});

extract!(build, |f| {
    let ident = &f.ident;
    if field_is(f, "Option") {
        quote! {
            #ident: self.#ident.take(),
        }
    } else if field_is(f, "Vec") {
        quote! {
            #ident: std::mem::replace(&mut self.#ident, vec![]),
        }
    } else {
        quote! {
            #ident: self.#ident.take().unwrap(),
        }
    }
});

fn field_is(field: &syn::Field, type_name: &str) -> bool {
    match &field.ty {
        syn::Type::Path(p) => p
            .path
            .segments
            .last()
            .map(|pair| pair.ident == type_name)
            .unwrap_or(false),
        _ => unimplemented!(),
    }
}

fn get_field_meta_items(attr: &syn::Attribute) -> Option<Vec<syn::NestedMeta>> {
    if attr.path.is_ident("builder") {
        match attr.parse_meta() {
            Ok(syn::Meta::List(ref meta)) => Some(meta.nested.iter().cloned().collect()),
            _ => {
                // TODO: produce an error
                None
            }
        }
    } else {
        None
    }
}

fn get_inner_generic_type(field: &syn::Field) -> &syn::Type {
    match &field.ty {
        syn::Type::Path(path) => {
            path.path.segments.last().map(|seg| match &seg.arguments {
                syn::PathArguments::AngleBracketed(args) => match args.args.first().unwrap() {
                    syn::GenericArgument::Type(ty) => ty,
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            })
        }
        .unwrap(),
        _ => unimplemented!(),
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident;
    let build_name = format!("{}Builder", ident);
    let builder = syn::Ident::new(&build_name, ident.span());

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let data = if let syn::Data::Struct(ref s) = input.data {
        s
    } else {
        panic!("Builder only support struct type")
    };

    let _ = if let syn::Fields::Named(ref n) = data.fields {
        n
    } else {
        panic!("Builder does not support new struct yet");
    };

    let fields = fields(&data);
    let init_fields = init_fields(&data);
    let setters = setters(&data);
    let check_fields = check_fields(&data);
    let build = build(&data);

    let input = quote! {
        pub struct #builder #ty_generics {
            #( #fields )*
        }

        impl #impl_generics #ident #ty_generics #where_clause {
            pub fn builder() -> #builder #ty_generics {
                #builder {
                    #( #init_fields )*
                }
            }
        }

        impl #impl_generics #builder #ty_generics #where_clause {

            #( #setters )*

            pub fn build(&mut self) -> Result<#ident #ty_generics, Box<dyn std::error::Error>> {
                #( #check_fields )*

                Ok(#ident {
                    #( #build )*
                })
            }
        }
    };

    input.into()
}
