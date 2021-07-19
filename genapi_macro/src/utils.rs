use std::iter::{FromIterator, Map};

use super::record::{Record, Rfield};
use proc_macro2::Span;
use syn::{
    punctuated::Punctuated,
    token::{Colon, Pub, Star},
    Expr, ExprArray, ExprStruct, Field, FieldValue, Ident, Lit, Member, Path, PathArguments,
    PathSegment, Type, TypePath, VisPublic, Visibility,
};

pub fn mk_ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

pub fn mk_field(name: &str) -> Field {
    Field {
        attrs: vec![],
        vis: Visibility::Public(VisPublic {
            pub_token: Pub(Span::call_site()),
        }),
        ident: Some(mk_ident(name)),
        colon_token: Some(Colon(Span::call_site())),
        ty: Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: None,
                segments: Punctuated::from_iter(vec![
                    PathSegment {
                        ident: mk_ident("std"),
                        arguments: PathArguments::None,
                    },
                    PathSegment {
                        ident: mk_ident("string"),
                        arguments: PathArguments::None,
                    },
                    PathSegment {
                        ident: mk_ident("String"),
                        arguments: PathArguments::None,
                    },
                ]),
            },
        }),
    }
}

pub fn gt_first_field(expr_struct: &ExprStruct) -> String {
    let field = expr_struct.fields.first();
    let field_value = field.map(|f| &f.expr).unwrap();
    match field_value {
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(str_lit) => str_lit.value(),
            _ => panic!(),
        },
        _ => panic!(),
    }
}

pub fn gt_field_by_name<'a>(
    expr_struct: &'a ExprStruct,
    field_name: &str,
) -> Option<&'a FieldValue> {
    expr_struct.fields.iter().find(|field| match &field.member {
        Member::Named(name) => name == field_name,
        Member::Unnamed(_) => false,
    })
}
