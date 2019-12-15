#![recursion_limit = "256"]

extern crate proc_macro;

use darling::FromMeta;
use quote::{quote, ToTokens};
use std::hash::{Hash, Hasher};
use syn::*;
use twox_hash::XxHash;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = XxHash::default();
    t.hash(&mut s);
    s.finish()
}

fn get_last_type_path_segment(ty: &Type) -> Option<&PathSegment> {
    if let Type::Path(TypePath {
        path: Path { ref segments, .. },
        ..
    }) = *ty
    {
        segments.last()
    } else {
        None
    }
}

#[derive(Debug, FromMeta)]
struct MacroArgs {
    #[darling(default)]
    cache: bool,
}

#[proc_macro_attribute]
pub fn snoozy(
    attr: proc_macro::TokenStream,
    mut input_tokens: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = input_tokens.clone();
    let input = parse_macro_input!(input as ItemFn);
    let code_hash = calculate_hash(&input);
    let code_hash = quote! { #code_hash };
    //let code_hash = LitInt::new(format!("{}u64", calculate_hash(&input)), Span::call_site());

    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let attr = match MacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return e.write_errors().into();
        }
    };

    let mut impl_fn = input.clone();

    // Used in the quasi-quotation below as `#name`.
    let impl_fn_name = input.sig.ident.clone();
    let impl_fn_name_str = impl_fn_name.to_string();

    // The implementation function contains a suffix which we strip for the user-facing function
    // This is necessary to get correct line numbers for errors while https://github.com/rust-lang/rust/issues/43081 is unresolved.
    let name = Ident::new(
        {
            let name = &impl_fn_name_str;
            let suffix = "_snoozy";
            let suffix_len = suffix.len();
            assert!(
                name.len() > suffix_len,
                "Function name must contain a '_snoozy' suffix"
            );
            assert!(
                &name[name.len() - suffix_len..] == suffix,
                "Function name must contain a '_snoozy' suffix"
            );
            &name[..name.len() - suffix_len]
        },
        impl_fn_name.span(),
    );

    let payload_name = Ident::new(&format!("{}_Payload", name), name.span());
    let fn_visibility = input.vis.clone();

    /*let ctx_arg = proc_macro::TokenStream::from(quote! {herpderp: f32});
    input
        .decl
        .inputs
        .insert(0, parse_macro_input!(ctx_arg as FnArg));*/
    //input.sig.inputs.insert(0, parse_quote! {herpderp: f32});

    let mut param_struct_fields: Vec<Field> = vec![];

    let mut context_arg_found = false;
    let mut recipe_arg_idents = Vec::new();

    for arg in input.sig.inputs.iter() {
        if let FnArg::Typed(ref arg) = arg {
            let ident = if let Pat::Ident(ref ident) = *arg.pat {
                ident.ident.clone()
            } else {
                panic!("onoz, non-ident fn arg");
            };

            // The first parameter should be a mutable reference to a snoozy Context struct
            // Let's first get through the reference.
            if let Type::Reference(TypeReference { elem: ref ty, .. }) = *arg.ty {
                // Now that we're at the type, get its path (like foo::bar::baz)
                recipe_arg_idents.push(ident.clone());

                param_struct_fields.push(Field {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    ident: Some(ident),
                    colon_token: Some(parse_quote! {:}),
                    ty: (**ty).clone(),
                });
            } else if let Type::Path(TypePath {
                path: Path { ref segments, .. },
                ..
            }) = *arg.ty
            {
                // Finally, check that the last component is "Context"
                if let Some(PathSegment { ref ident, .. }) = segments.last() {
                    if ident.to_string() == "Context" {
                        context_arg_found = true;
                        continue;
                    }
                }
            } else {
                panic!("All arguments to a snoozy function must be references");
            }
        }
    }

    if !context_arg_found {
        panic!(
            "The first argument to a snoozy function should be ctx: Context. Found: {:?}",
            input.sig.inputs[0].clone().into_token_stream().to_string()
        );
    }

    let generics = input.sig.generics;
    let output_type: Type = if let ReturnType::Type(_, ref ty) = &input.sig.output {
        if let Some(ret_type) = get_last_type_path_segment(ty) {
            if ret_type.ident.to_string() != "Result" {
                panic!("The return type of snoozy functions must be Result<_>");
            }

            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref args, ..
            }) = ret_type.arguments
            {
                if let GenericArgument::Type(ty) =
                    args.first().expect("getting the type parameter of Result")
                {
                    ty.clone()
                } else {
                    panic!("Expected a single type parameter to Result");
                }
            } else {
                panic!("Expected a single type parameter to Result");
            }
        } else {
            panic!("The return type of snoozy functions must be Result<_>");
        }
    } else {
        panic!("Snoozy functions must return Result<_>");
    };

    let (impl_generics, ty_generics, where_generics) = generics.split_for_impl();

    let recipe_op_impl_name = Ident::new(&format!("{}_snoozy_impl", name), name.span());
    let recipe_forward_idents = recipe_arg_idents.clone();

    let payload_ctor_args = param_struct_fields.clone();
    let payload_ctor_forwards = recipe_forward_idents.clone();

    let synth_fn_args = param_struct_fields.clone();
    let main_def_arg_forwards = recipe_forward_idents.clone();

    let generic_types_hash = if generics.type_params().next().is_none() {
        quote! { 0 }
    } else {
        let types = generics.type_params().map(|p| p.ident.clone());
        quote! { snoozy::get_type_hash::<(#(#types),*)>() }
    };

    impl_fn.sig.ident = recipe_op_impl_name.clone();
    let should_cache_result = attr.cache;

    let expanded = quote! {
        #[allow(non_camel_case_types)]
        #fn_visibility struct #payload_name #generics {
            #(pub #param_struct_fields,)*
        }

        #[allow(non_camel_case_types)]
        #fn_visibility struct #name #generics {
            payload: std::sync::Arc<#payload_name #ty_generics>,
        }

        impl #impl_generics std::hash::Hash for #name #ty_generics where #where_generics {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                #generic_types_hash.hash(state);
                #code_hash.hash(state);
                #(whatever_hash(&self.payload.#recipe_arg_idents, state);)*
            }
        }

        impl #impl_generics #name #ty_generics where #where_generics {
            #fn_visibility fn new(#(#payload_ctor_args,)*) -> Self {
                Self {
                    payload: std::sync::Arc::new(#payload_name {
                        #(#payload_ctor_forwards),*
                    })
                }
            }
        }

        #fn_visibility fn #name #generics (#(#synth_fn_args,)*) -> SnoozyRef<#output_type> {
            snoozy_def_binding(#name {
                payload: std::sync::Arc::new(#payload_name {
                    #(#main_def_arg_forwards),*
                })
            })
        }

        impl #impl_generics Op for #name #ty_generics where #where_generics {
            type Res = #output_type;

            //fn run(&self, ctx: &mut Context) -> Future<Result<#output_type>> {
           //fn run<'a>(&'a self, ctx: &mut Context) -> std::pin::Pin<Box<dyn snoozy::futures::Future<Output = Result<Self::Res>> + Send + 'a>> {
            fn run<'a>(&'a self, mut ctx: Context) -> std::pin::Pin<Box<dyn snoozy::futures::Future<Output = Result<Self::Res>> + Send + 'a>> {
                use snoozy::futures::future::FutureExt;

                let payload = self.payload.clone();
                async move {
                    #impl_fn_name(ctx, #(&payload.#recipe_forward_idents),*).await
                }.boxed()
            }

            fn name() -> &'static str {
                stringify!(#name)
            }

            fn should_cache_result(&self) -> bool {
                #should_cache_result
            }
        }
    };

    input_tokens.extend(proc_macro::TokenStream::from(expanded));

    /*let mut result = proc_macro::TokenStream::from(expanded);
    let mut module_inner: proc_macro::TokenStream = quote! { use super::*; }.into();
    //let impl_fn = impl_fn.into_token_stream();
    //let impl_fn: proc_macro::TokenStream = impl_fn.into();
    let impl_module = {
        use core::iter::FromIterator;
        use proc_macro::*;

        //panic!(format!(module_inner));
        module_inner.extend(input_tokens);

        let impl_module: Vec<TokenTree> = vec![
            TokenTree::Ident(Ident::new("mod", Span::call_site())),
            TokenTree::Ident(Ident::new(&impl_mod_name, Span::call_site())),
            TokenTree::Group(Group::new(Delimiter::Brace, module_inner)),
        ];
        TokenStream::from_iter(impl_module.into_iter())
    };*/

    //result.extend(impl_module);
    input_tokens
}
