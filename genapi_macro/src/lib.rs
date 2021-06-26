#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![feature(vec_into_raw_parts)]
#![feature(type_ascription)]
#![feature(concat_idents)]
extern crate proc_macro;

use std::iter::FromIterator;
use std::path::Path;

use proc_macro::{Span, TokenStream};
use proc_macro2::Span as Sp;
use proc_macro2::TokenStream as Ts;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
use syn::token::Colon;
use syn::token::Colon2;
use syn::token::Pub;
use syn::PathArguments;
use syn::PathSegment;
use syn::TypePath;
use syn::VisPublic;
use syn::{
    braced, parse_macro_input, token, Expr, ExprArray, ExprStruct, Field, Ident, Item, Lit,
    Path as OtherPath, Result, Token, Type, Visibility,
};
use syn::{
    parse::{Parse, ParseStream},
    token::Token,
};
use syn::{punctuated::Punctuated, Member};

mod utils;

#[proc_macro]
pub fn create(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ExprStruct);
    impl_create(ast)
}

#[proc_macro]
pub fn ignite(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ExprArray);
    impl_ignite(ast)
}

#[proc_macro]
pub fn create_model(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ExprStruct);
    impl_create_model(ast)
}

fn impl_ignite(ast: ExprArray) -> TokenStream {
    dbg!(&ast);
    let gen = quote! {
        rocket::ignite().mount("/", routes!#ast).launch();
    };
    gen.into()
}

fn impl_create(ast: ExprStruct) -> TokenStream {
    let path_string = utils::gt_first_field(&ast);

    let mut it = ast.fields.iter();
    it.next();
    let m = it.next();

    // let method_field = ast.fields.second();
    let method_value = m.map(|f| &f.expr).unwrap();
    let method_variant = match method_value {
        Expr::Path(expr_path) => expr_path.path.segments.last().unwrap().ident.clone(),
        _ => panic!(),
    };

    let response_field = ast.fields.last();
    let response_value = response_field.map(|f| &f.expr).unwrap();

    let fn_ident = utils::mk_ident(&path_string);
    let path_attr = format!("/{}", path_string).to_string();

    // THIS IS THE OUTPUT YOU WANT TO PRODUCE
    let gen = quote! {

        #[get(#path_attr)]
        fn #fn_ident() -> &'static str {
            #response_value.body
        }
    };

    gen.into()
}

fn impl_create_model(ast: ExprStruct) -> TokenStream {
    let mut name_string = utils::gt_first_field(&ast);
    let struct_name_ident = utils::mk_ident(&name_string);
    let mut table_name = String::from(&name_string);
    table_name.push_str("s");
    name_string.insert_str(0, "New");
    let new_struct_name_ident = utils::mk_ident(&name_string);

    let fields_field = utils::gt_field_by_name(&ast, "fields");
    let fields_value: Expr = fields_field.cloned().map(|f| f.expr).unwrap();

    // TODO figure out what we are doing here
    let fields_array = match &fields_value {
        Expr::Array(expr_array) => expr_array.elems.iter().map(|field: &Expr| match field {
            Expr::Struct(expr_struct) => {
                let field_name = utils::gt_first_field(expr_struct);
                utils::mk_field(&field_name)
            }
            _ => panic!("Not Expression Struct"),
        }),
        _ => panic!(),
    };

    let fields_array2 = fields_array.clone();

    let gen = quote! {

        #[derive(Debug, Queryable, Serialize, Deserialize)]
        pub struct #struct_name_ident {
            pub id: i32,
            #(#fields_array),*
        }

        #[derive(Debug, Insertable, AsChangeset, Serialize, Deserialize)]
        #[table_name = #table_name]
          pub struct #new_struct_name_ident {
            #(#fields_array2),*
          }


    };

    gen.into()
}
