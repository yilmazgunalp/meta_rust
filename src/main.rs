
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![feature(proc_macro_hygiene, decl_macro)]
#![feature(concat_idents)]
#![feature(type_ascription)]
#[macro_use] extern crate rocket;
use genapi_macro::create;
extern crate genapi_macro;
use quote::quote;
use quote::{ToTokens,TokenStreamExt};
use proc_macro2::{Span, TokenStream};
use syn::{Field, Ident, Item, Result, Token, braced, parse_macro_input, token};
use proc_macro2::TokenStream as Ts;
use proc_macro2::Span as Sp;

    

//WHAT YOU ARE LOOKING AT IS THE ACTUAL OUTPUT API MAIN FILE
fn main() {
    println!("Hello, world!");
    
    //Can I maybe write this bit with a template???
    genapi_macro::create!(Endpoint {path: "ruya", method: Method::get, response: Response {
        content_type: ContentType::TEXT,
        body: "yahooooola"
    }});
    genapi_macro::create!(Endpoint {path: "you", method: Method::get, response: Response {
        content_type: ContentType::TEXT,
        body: "yahooooola"
    }});
    genapi_macro::ignite!([ruya,you,]);

}

#[derive(Debug)] 
struct Endpoint<'a> { 
    path: &'a str,
    method: Method,
    response: Response<'a>,
}



#[derive(Debug)]
enum Method {
    get,
    POST,
}

#[derive(Debug)]
struct Response<'a> {
    content_type: ContentType,
    body: &'a str,
}

#[derive(Debug)]
enum ContentType {
    JSON,
    TEXT,
}


