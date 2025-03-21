#![feature(try_find, proc_macro_expand)]

use proc_macro::TokenStream;
use proc_macro_deterministic_rand::{RandomSource, keys_from_cargo};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::ToTokens;
use syn::{
    Abi, BareFnArg, BareVariadic, Error, Expr, FnArg, Ident, ItemFn, LitStr, Meta, MetaList, Token,
    TypeBareFn,
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
};

enum ExportName {
    Implicit(Span),
    Specified(Span, Expr),
}

impl ExportName {
    fn span(&self) -> Span {
        match self {
            Self::Implicit(span) => *span,
            Self::Specified(span, _) => *span,
        }
    }
}

fn as_export_name(attr: &Meta) -> Option<(ExportName, bool)> {
    match attr {
        Meta::Path(p) if p.is_ident("no_mangle") => {
            Some((ExportName::Implicit(attr.span()), false))
        }
        Meta::NameValue(p) if p.path.is_ident("export_name") => {
            Some((ExportName::Specified(attr.span(), p.value.clone()), false))
        }
        Meta::List(p) if p.path.is_ident("unsafe") => {
            let meta = p.parse_args::<Meta>().ok()?;
            as_export_name(&meta).map(|(v, _)| (v, true))
        }
        _ => None,
    }
}

fn is_cfg(attr: &Meta) -> bool {
    match attr {
        Meta::List(l) if l.path.is_ident("cfg") => true,
        _ => false,
    }
}

fn is_doc(attr: &Meta) -> bool {
    match attr {
        Meta::List(l) if l.path.is_ident("doc") => true,
        Meta::NameValue(l)
            if l.path.is_ident("doc")
                || l.path.is_ident("deprecated")
                || l.path.is_ident("must_use") =>
        {
            true
        }
        Meta::Path(p)
            if p.is_ident("deprecated") || p.is_ident("must_use") || p.is_ident("cold") =>
        {
            true
        }
        _ => false,
    }
}

fn is_lint(attr: &Meta) -> bool {
    match attr {
        Meta::List(l)
            if l.path.is_ident("allow")
                || l.path.is_ident("warn")
                || l.path.is_ident("deny")
                || l.path.is_ident("forbid")
                || l.path.is_ident("expect") =>
        {
            true
        }
        _ => false,
    }
}

fn expand_string_at(expansion: TokenStream2) -> syn::Result<String> {
    let span = expansion.span();
    let ts: TokenStream = expansion.into();

    let expanded = ts.expand_expr().map_err(|e| syn::Error::new(span, e))?;

    let lit: LitStr = syn::parse(expanded.into())?;

    Ok(lit.value())
}

#[derive(Default)]
struct IfuncAttributes {
    export_name: Option<ExportName>,
    export_name_has_unsafe: bool,
    cfg: TokenStream2,
    doc: TokenStream2,
    other_attrs: TokenStream2,
}

fn write_attr(attr: &Meta, list: &mut TokenStream2, cfg_predicate: Option<&Meta>) {
    if let Some(cfg_predicate) = cfg_predicate {
        list.extend(quote::quote! { #[cfg_attr(#cfg_predicate, #attr)]})
    } else {
        attr.to_tokens(list);
    }
}

fn visit_attr(
    attr: &Meta,
    attrs: &mut IfuncAttributes,
    cfg_predicate: Option<&Meta>,
) -> syn::Result<()> {
    if let Some((export_name, has_unsafe)) = as_export_name(&attr) {
        if let Some(cfg_predicate) = cfg_predicate {
            let ident = attr.path().get_ident().unwrap();
            return Err(syn::Error::new_spanned(
                attr,
                format_args!("cfg_attr({}) is not supported", ident),
            ));
        }
        attrs.export_name = Some(export_name);
        attrs.export_name_has_unsafe = has_unsafe;
    } else if is_cfg(attr) || is_lint(attr) {
        write_attr(attr, &mut attrs.cfg, cfg_predicate)
    } else if is_doc(attr) {
        write_attr(attr, &mut attrs.doc, cfg_predicate)
    } else {
        match attr {
            Meta::List(l) if l.path.is_ident("cfg_attr") => {
                let mut list = l
                    .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?
                    .into_iter();
                let cfg_predicate_n = list
                    .next()
                    .ok_or_else(|| syn::Error::new_spanned(l, "cfg_attr must have a predicate"))?;

                let cfg_predicate = if let Some(cfg_predicate) = cfg_predicate {
                    Meta::List(MetaList {
                        path: l.path.clone(),
                        delimiter: l.delimiter.clone(),
                        tokens: quote::quote!(all(#cfg_predicate, #cfg_predicate_n)),
                    })
                } else {
                    cfg_predicate_n
                };

                for nested in list {
                    visit_attr(&nested, attrs, Some(&cfg_predicate))?;
                }
            }
            _ => write_attr(attr, &mut attrs.other_attrs, cfg_predicate),
        }
    }

    Ok(())
}

fn parse_indirect_func(
    item: ItemFn,
    mut rand: RandomSource,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let sig = item.sig.clone();

    let vis = item.vis;

    let real_sig = item.sig;

    let mut attrs = IfuncAttributes::default();
    let name = sig.ident;

    let body = item.block;

    for attr in item.attrs {
        visit_attr(&attr.meta, &mut attrs, None)?;
    }

    let IfuncAttributes {
        export_name,
        export_name_has_unsafe,
        cfg,
        doc,
        other_attrs,
    } = attrs;

    let block_abi = sig.abi.clone().unwrap_or(Abi {
        extern_token: Default::default(),
        name: Some(LitStr::new("Rust", Span::mixed_site())),
    });

    let fn_ty: TypeBareFn = TypeBareFn {
        lifetimes: None,
        unsafety: sig.unsafety,
        abi: sig.abi.clone(),
        fn_token: sig.fn_token,
        paren_token: sig.paren_token,
        inputs: sig
            .inputs
            .into_pairs()
            .map(|p| {
                let (a, punct) = p.into_tuple();

                let b = match a {
                    FnArg::Receiver(s) => {
                        return Err(Error::new(
                            s.span(),
                            "Cannot use `#[indirect_func]` on a method with a reciever",
                        ));
                    }
                    FnArg::Typed(ty) => BareFnArg {
                        attrs: ty.attrs,
                        name: None,
                        ty: *ty.ty,
                    },
                };

                Ok(Pair::new(b, punct))
            })
            .collect::<syn::Result<_>>()?,
        variadic: sig.variadic.map(|v| BareVariadic {
            attrs: v.attrs,
            name: None,
            dots: v.dots,
            comma: None,
        }),
        output: sig.output,
    };

    let safe = match sig.unsafety {
        Some(un) => quote::quote_spanned! {un.span => #un},
        None => quote::quote! { safe },
    };

    let resolver_name = Ident::new("__ifunc_resolver", name.span());

    let sym_name = if let Some(export_name) = export_name {
        if !export_name_has_unsafe {
            return Err(syn::Error::new(
                export_name.span(),
                "Must use `unsafe` attribute with no_mangle/export_name",
            ));
        }
        match export_name {
            ExportName::Implicit(span) => {
                let name = LitStr::new(&format!("{name}"), span);
                quote::quote! {#name}
            }
            ExportName::Specified(_, name) => quote::quote! {#name},
        }
    } else {
        let val = rand.next(name.span());

        let prefix =
            expand_string_at(quote::quote_spanned! {name.span() => ::core::module_path!()})?;

        let prefix = prefix.replace("::", ".");

        let name_str = format!(".{prefix}.{name}.__IFR__.N{val:016x}");
        let name = LitStr::new(&name_str, name.span());
        quote::quote! {#name}
    };

    Ok(quote::quote! {

        unsafe #block_abi {
            #cfg
            #doc
            #[link_name = #sym_name]
            #vis #safe #real_sig;
        }

        const _: () = {
            #cfg
            mod __internal {

                use super::*;

                #other_attrs
                #[allow(improper_ctypes_definitions)]
                extern "C" fn #resolver_name () -> #fn_ty #body
                ::ifunc::create_resolver!{
                    #sym_name, #resolver_name
                }
            }

        };
    })
}

#[proc_macro_attribute]
pub fn indirect_func(_input: TokenStream, item: TokenStream) -> TokenStream {
    let item = syn::parse_macro_input!(item as syn::ItemFn);

    let key_span = item.span();

    let rand = RandomSource::with_key_span_and_seed(key_span, keys_from_cargo!("indirect_func"));

    let res = match parse_indirect_func(item, rand) {
        Ok(item) => item,
        Err(e) => e.into_compile_error(),
    };

    res.into()
}
