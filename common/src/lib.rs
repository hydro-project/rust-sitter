use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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
            input.parse_terminated(NameValueExpr::parse)?
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

pub fn try_extract_inner_type(ty: &Type, inner_of: &str) -> (Type, bool) {
    if let Type::Path(p) = &ty {
        let type_segment = p.path.segments.first().unwrap();
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
        } else {
            (ty.clone(), false)
        }
    } else {
        (ty.clone(), false)
    }
}
