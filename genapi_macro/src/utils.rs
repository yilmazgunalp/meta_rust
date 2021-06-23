use std::iter::FromIterator;

use proc_macro2::Span;
use syn::{
    punctuated::Punctuated,
    token::{Colon, Pub},
    Field, Ident, Path, PathArguments, PathSegment, Type, TypePath, VisPublic, Visibility,
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
