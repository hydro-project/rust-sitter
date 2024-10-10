use std::collections::HashSet;

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameValueExpr {
    pub path: Ident,
    pub eq_token: Token![=],
    pub expr: Expr,
}

impl Parse for NameValueExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(NameValueExpr {
            path: input.parse()?,
            eq_token: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldThenParams {
    pub field: Field,
    pub comma: Option<Token![,]>,
    pub params: Punctuated<NameValueExpr, Token![,]>,
}

impl Parse for FieldThenParams {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let field = Field::parse_unnamed(input)?;
        let comma: Option<Token![,]> = input.parse()?;
        let params = if comma.is_some() {
            Punctuated::parse_terminated_with(input, NameValueExpr::parse)?
        } else {
            Punctuated::new()
        };

        Ok(FieldThenParams {
            field,
            comma,
            params,
        })
    }
}

pub fn try_extract_inner_type(
    ty: &Type,
    inner_of: &str,
    skip_over: &HashSet<&str>,
) -> (Type, bool) {
    if let Type::Path(p) = &ty {
        let type_segment = p.path.segments.last().unwrap();
        if type_segment.ident == inner_of {
            let leaf_type = if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    t
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            };

            (leaf_type, true)
        } else if skip_over.contains(type_segment.ident.to_string().as_str()) {
            if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    try_extract_inner_type(&t, inner_of, skip_over)
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            }
        } else {
            (ty.clone(), false)
        }
    } else {
        (ty.clone(), false)
    }
}

pub fn filter_inner_type(ty: &Type, skip_over: &HashSet<&str>) -> Type {
    if let Type::Path(p) = &ty {
        let type_segment = p.path.segments.last().unwrap();
        if skip_over.contains(type_segment.ident.to_string().as_str()) {
            if let PathArguments::AngleBracketed(p) = &type_segment.arguments {
                if let GenericArgument::Type(t) = p.args.first().unwrap().clone() {
                    filter_inner_type(&t, skip_over)
                } else {
                    panic!("Argument in angle brackets must be a type")
                }
            } else {
                panic!("Expected angle bracketed path");
            }
        } else {
            ty.clone()
        }
    } else {
        ty.clone()
    }
}

pub fn wrap_leaf_type(ty: &Type, skip_over: &HashSet<&str>) -> Type {
    let mut ty = ty.clone();
    if let Type::Path(p) = &mut ty {
        let type_segment = p.path.segments.last_mut().unwrap();
        if skip_over.contains(type_segment.ident.to_string().as_str()) {
            if let PathArguments::AngleBracketed(args) = &mut type_segment.arguments {
                for a in args.args.iter_mut() {
                    if let syn::GenericArgument::Type(t) = a {
                        *t = wrap_leaf_type(t, skip_over);
                    }
                }

                ty
            } else {
                panic!("Expected angle bracketed path");
            }
        } else {
            parse_quote!(rust_sitter::WithLeaf<#ty>)
        }
    } else {
        parse_quote!(rust_sitter::WithLeaf<#ty>)
    }
}
