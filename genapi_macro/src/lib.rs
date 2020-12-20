#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![feature(vec_into_raw_parts)]
#![feature(type_ascription)]
#![feature(concat_idents)]
extern crate proc_macro;

use proc_macro::{Span, TokenStream};
use proc_macro2::TokenStream as Ts;
use proc_macro2::Span as Sp;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
use syn::{Expr, ExprArray, ExprStruct, Field, Ident, Item, Lit, Result, Token, braced, parse_macro_input, token};
use syn::{token::Token, parse::{Parse, ParseStream}};
use syn::punctuated::Punctuated;



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

fn impl_ignite(ast: ExprArray) -> TokenStream {
    
   dbg!(&ast);
    let gen = quote! {  
        
     
        rocket::ignite().mount("/", routes!#ast).launch();

    };

    
    gen.into()

}

fn impl_create(ast: ExprStruct) -> TokenStream {
    
    let path_field = ast.fields.first();
    let path_value = path_field.map(|f| {&f.expr}).unwrap();
    let path_string = match path_value {
        Expr::Lit(expr_lit) => {
            match &expr_lit.lit {
                Lit::Str(str_lit) => str_lit.value(),
                _ => panic!()

            }
        },
        _ => panic!()
    };

    let mut it = ast.fields.iter();
    it.next();
    let m = it.next();

    // let method_field = ast.fields.second();
    let method_value = m.map(|f| {&f.expr}).unwrap();
    let method_variant = match method_value {
        Expr::Path(expr_path) =>  expr_path.path.segments.last().unwrap().ident.clone(),
        _ => panic!()
    };
  

    let response_field = ast.fields.last();
    let response_value = response_field.map(|f| {&f.expr}).unwrap();

    let fn_ident = Ident::new(&path_string, Sp::call_site());
    let path_attr = format!("/{}",path_string).to_string();

    // THIS IS THE OUTPUT YOU WANT TO PRODUCE   
    let gen = quote! {  
        
        #[get(#path_attr)] 
        fn #fn_ident() -> &'static str {
            #response_value.body
        }
        // rocket::ignite().mount("/", routes![#fn_ident]).launch();

    };

    
    gen.into()

}
