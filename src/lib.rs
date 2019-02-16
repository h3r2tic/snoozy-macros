#![recursion_limit = "128"]

extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use std::hash::{Hash, Hasher};
use syn::*;
use twox_hash::XxHash;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = XxHash::default();
    t.hash(&mut s);
    s.finish()
}

fn get_first_type_path_segment(ty: &Type) -> Option<&PathSegment> {
    if let Type::Path(TypePath {
        path: Path { ref segments, .. },
        ..
    }) = *ty
    {
        // Finally, check that the last component is "Context"
        if let Some(punctuated::Pair::End(ps @ PathSegment { .. })) = segments.last() {
            return Some(&ps);
        }
    }

    None
}

#[proc_macro_attribute]
pub fn snoozy(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let code_hash = LitInt::new(calculate_hash(&input), IntSuffix::U64, Span::call_site());

    let mut impl_fn = input.clone();

    // Used in the quasi-quotation below as `#name`.
    let name = input.ident.clone();
    let fn_visibility = input.vis.clone();

    /*let ctx_arg = proc_macro::TokenStream::from(quote! {herpderp: f32});
    input
        .decl
        .inputs
        .insert(0, parse_macro_input!(ctx_arg as FnArg));*/
    //input.decl.inputs.insert(0, parse_quote! {herpderp: f32});

    let mut param_struct_fields: Vec<Field> = vec![];

    let mut context_arg_found = false;
    let mut recipe_arg_idents = Vec::new();

    for arg in input.decl.inputs.iter() {
        if let FnArg::Captured(ref arg) = arg {
            let ident = if let Pat::Ident(ref ident) = arg.pat {
                ident.ident.clone()
            } else {
                panic!("onoz, non-ident fn arg");
            };

            // The first parameter should be a mutable reference to a snoozy Context struct
            // Let's first get through the reference.
            if let Type::Reference(TypeReference { elem: ref ty, .. }) = arg.ty {
                // Now that we're at the type, get its path (like foo::bar::baz)
                if let Type::Path(TypePath {
                    path: Path { ref segments, .. },
                    ..
                }) = **ty
                {
                    // Finally, check that the last component is "Context"
                    if let Some(punctuated::Pair::End(PathSegment { ref ident, .. })) =
                        segments.last()
                    {
                        if ident.to_string() == "Context" {
                            context_arg_found = true;
                            continue;
                        }
                    }
                }

                recipe_arg_idents.push(ident.clone());

                param_struct_fields.push(Field {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    ident: Some(ident),
                    colon_token: Some(parse_quote! {:}),
                    ty: (**ty).clone(),
                });
            } else {
                panic!("All arguments to a snoozy function must be references");
            }
        }
    }

    if !context_arg_found {
        panic!(
            "The first argument to a snoozy function should be &Context. Found: {:?}",
            input.decl.inputs[0].clone().into_token_stream().to_string()
        );
    }

    let generics = input.decl.generics;
    let output_type: Type = if let ReturnType::Type(_, ref ty) = &input.decl.output {
        if let Some(ret_type) = get_first_type_path_segment(ty) {
            if ret_type.ident.to_string() != "Result" {
                panic!("The return type of snoozy functions must be Result<_>");
            }

            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref args, ..
            }) = ret_type.arguments
            {
                if let GenericArgument::Type(ty) = args
                    .first()
                    .expect("getting the type parameter of Result")
                    .value()
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

    impl_fn.ident = recipe_op_impl_name.clone();

    let expanded = quote! {
        #[allow(non_camel_case_types)]
        #fn_visibility struct #name #generics {
            #(pub #param_struct_fields,)*
        }

        impl #impl_generics RecipeHash for #name #ty_generics where #where_generics {
            fn recipe_hash(&self) -> u64 {
                #generic_types_hash ^
                #code_hash #(^ calculate_serialized_hash(&self.#recipe_arg_idents))*
            }
        }

        impl #impl_generics #name #ty_generics where #where_generics {
            #fn_visibility fn new(#(#payload_ctor_args,)*) -> Self {
                Self {
                    #(#payload_ctor_forwards),*
                }
            }
        }

        #fn_visibility fn #name #generics (#(#synth_fn_args,)*) -> SnoozyRef<#output_type> {
            let op_source_hash: u64 = #generic_types_hash ^ #code_hash;
            def(#name { #(#main_def_arg_forwards),* }, op_source_hash)
        }

        impl #impl_generics Op for #name #ty_generics where #where_generics {
            type Res = #output_type;

            fn run(&self, ctx: &mut Context) -> Result<#output_type> {
                #recipe_op_impl_name(ctx, #(&self.#recipe_forward_idents),*)
            }
        }

        impl #impl_generics SnoozyNamedOp for #name #ty_generics where #where_generics {
            type Res = #output_type;

            fn def_named_initial(identity_hash: u64, init_value: Self) -> SnoozyRef<Self::Res> {
                def_initial(identity_hash, init_value)
            }

            fn redef_named(identity_hash: u64, new_value: Self) {
                def_named(identity_hash, new_value);
            }
        }

        #impl_fn
    };

    proc_macro::TokenStream::from(expanded)
}
