use quote::quote;
use synstructure::{decl_derive, Structure};

decl_derive!([Mark, attributes(ignore_mark)] => derive_trace);

fn derive_trace(mut s: Structure<'_>) -> proc_macro2::TokenStream {
    s.filter(|bi| {
        !bi.ast()
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("ignore_mark"))
    });
    let trace_body = s.each(|bi| quote!(mark(#bi)));

    let trace_impl = s.bound_impl(
        quote!(::safe_gc::Mark),
        quote! {
            #[inline] fn mark_all(&self) {
                #[allow(dead_code)]
                #[inline]
                fn mark<T: ::safe_gc::Mark>(it: &T) {
                    ::safe_gc::Mark::mark_all(it);
                }
                match *self { #trace_body }
            }

            fn unroot(&self) {
                unimplemented!()
            }

            fn root(&self) {
                unimplemented!()
            }
        },
    );

    quote! {
        #trace_impl
    }
}