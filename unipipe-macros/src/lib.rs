use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    AngleBracketedGenericArguments, FnArg, GenericParam, Generics, Ident, ImplGenerics, ImplItem,
    ImplItemFn, ItemImpl, Pat, PatType, PathArguments, Token, Type, TypeGenerics, Visibility,
    WhereClause, WherePredicate,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::Comma,
};

struct UniPipeArgs {
    visibility: Option<Visibility>,
    extensions: Vec<Ident>,
}

impl Parse for UniPipeArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let visibility = if input.peek(Token![pub]) || input.peek(Token![crate]) {
            Some(input.parse()?)
        } else {
            None
        };

        let extensions = Punctuated::<Ident, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect();

        Ok(UniPipeArgs {
            visibility,
            extensions,
        })
    }
}

#[proc_macro_attribute]
pub fn unipipe(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as UniPipeArgs);
    let input = parse_macro_input!(item as ItemImpl);

    let (struct_name, impl_struct_generics, impl_struct_impl_generics) =
        match &input.self_ty.as_ref() {
            Type::Path(type_path) => {
                let segment = type_path
                    .path
                    .segments
                    .last()
                    .expect("Expected struct name");

                let struct_generics =
                    if let PathArguments::AngleBracketed(generics) = &segment.arguments {
                        Some(generics)
                    } else {
                        None
                    };

                (&segment.ident, struct_generics, &input.generics)
            }
            _ => panic!("Expected a simple struct name in impl"),
        };

    let mut constructor_methods = Vec::new();

    for item in &input.items {
        if let ImplItem::Fn(method) = item {
            if method.vis == Visibility::Inherited || matches!(method.vis, Visibility::Public(_)) {
                constructor_methods.push(method);
            }
        }
    }

    if constructor_methods.is_empty() {
        panic!("No public constructor methods found in impl block");
    }

    let visibility = args.visibility.unwrap_or_else(|| parse_quote!(pub));

    let mut output = quote! { #input };

    for extension_type in args.extensions {
        let extension = match extension_type.to_string().as_str() {
            "iterator" => IteratorExtension::generate(
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            "try_iterator" => TryIteratorExtension::generate(
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            "stream" => StreamExtension::generate(
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            "try_stream" => TryStreamExtension::generate(
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            _ => panic!("Unknown extension type: {}", extension_type),
        };

        output.extend(extension);
    }

    output.into()
}

trait Extension {
    fn get_name() -> &'static str;

    fn get_pipe_method_name_prefix() -> &'static str;

    fn get_extra_trait_params() -> Vec<proc_macro2::TokenStream>;

    fn get_extra_impl_for_params() -> Vec<proc_macro2::TokenStream>;

    fn generate_trait(
        visibility: &Visibility,
        trait_name: &Ident,
        trait_generics: &ImplGenerics,
        trait_ty_generics: &TypeGenerics,
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream;

    fn generate_trait_method(
        pipe_method_name: &Ident,
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream;

    fn generate(
        visibility: &Visibility,
        struct_name: &Ident,
        impl_struct_generics: Option<&AngleBracketedGenericArguments>,
        impl_struct_impl_generics: &Generics,
        constructor_methods: &[&ImplItemFn],
    ) -> proc_macro2::TokenStream {
        let trait_name = format_ident!("{}UniPipe{}Ext", struct_name, Self::get_name());

        let where_clause = impl_struct_impl_generics.where_clause.as_ref();

        let (struct_with_generics, struct_path_with_generics) =
            if impl_struct_impl_generics.params.is_empty() {
                (quote! { #struct_name }, quote! { #struct_name })
            } else {
                (
                    quote! { #struct_name #impl_struct_generics },
                    quote! { #struct_name::#impl_struct_generics },
                )
            };

        let trait_generics = {
            let mut generics = impl_struct_impl_generics.clone();

            let extra_params = Self::get_extra_trait_params();

            generics
                .params
                .extend::<Punctuated<GenericParam, Comma>>(parse_quote! {
                    #(#extra_params),*
                });

            generics
        };

        let impl_for_generics = {
            let mut generics = trait_generics.clone();

            let extra_params = Self::get_extra_impl_for_params();

            generics
                .params
                .extend::<Punctuated<GenericParam, Comma>>(parse_quote! {
                    #(#extra_params),*
                });

            generics
        };

        let (trait_generics, trait_ty_generics, _) = trait_generics.split_for_impl();

        let (impl_generics, _, _) = impl_for_generics.split_for_impl();

        let where_clause_predicates = where_clause.map(|clause| &clause.predicates);

        let mut trait_methods = Vec::new();

        for method in constructor_methods {
            let args: Vec<_> = method
                .sig
                .inputs
                .iter()
                .filter_map(|arg| match arg {
                    FnArg::Typed(pat_type) => Some(pat_type),
                    FnArg::Receiver(_) => None,
                })
                .collect();

            if args.len() != method.sig.inputs.len() {
                continue;
            }

            let arg_names: Vec<_> = args.iter().map(|arg| arg.pat.as_ref()).collect();

            let pipe_method_name = format_ident!(
                "{}{}",
                Self::get_pipe_method_name_prefix(),
                method_name_to_pipe_method(&method.sig.ident, struct_name)
            );

            trait_methods.push(Self::generate_trait_method(
                &pipe_method_name,
                &args,
                &arg_names,
                &struct_with_generics,
                &struct_path_with_generics,
                &method.sig.ident,
            ));
        }

        Self::generate_trait(
            visibility,
            &trait_name,
            &trait_generics,
            &trait_ty_generics,
            &struct_with_generics,
            &struct_path_with_generics,
            where_clause,
            where_clause_predicates,
            &impl_generics,
            trait_methods,
        )
    }
}

struct IteratorExtension;

impl Extension for IteratorExtension {
    fn get_name() -> &'static str {
        "Iterator"
    }

    fn get_pipe_method_name_prefix() -> &'static str {
        ""
    }

    fn get_extra_trait_params() -> Vec<proc_macro2::TokenStream> {
        vec![]
    }

    fn get_extra_impl_for_params() -> Vec<proc_macro2::TokenStream> {
        vec![quote! { TIterator }]
    }

    fn generate_trait(
        visibility: &Visibility,
        trait_name: &Ident,
        trait_generics: &ImplGenerics,
        trait_ty_generics: &TypeGenerics,
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                Iterator<Item = <#struct_with_generics as ::unipipe::UniPipe>::Input> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TIterator
            where
                TIterator: Iterator<Item = <#struct_path_with_generics as ::unipipe::UniPipe>::Input>,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = <#struct_with_generics as ::unipipe::UniPipe>::Output> {
                use ::unipipe::UniPipe as _;

                let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);
                let mut completed = false;

                std::iter::from_fn(move || {
                    if completed {
                        return None;
                    }

                    while let Some(input) = self.next() {
                        if let Some(output) = pipe.next(Some(input)) {
                            return Some(output);
                        }
                    }

                    completed = true;

                    if let Some(output) = pipe.next(None) {
                        return Some(output);
                    }

                    None
                })
            }
        }
    }
}

struct TryIteratorExtension;

impl Extension for TryIteratorExtension {
    fn get_name() -> &'static str {
        "TryIterator"
    }

    fn get_pipe_method_name_prefix() -> &'static str {
        "try_"
    }

    fn get_extra_trait_params() -> Vec<proc_macro2::TokenStream> {
        vec![quote! { TError }]
    }

    fn get_extra_impl_for_params() -> Vec<proc_macro2::TokenStream> {
        vec![quote! { TIterator }]
    }

    fn generate_trait(
        visibility: &Visibility,
        trait_name: &Ident,
        trait_generics: &ImplGenerics,
        trait_ty_generics: &TypeGenerics,
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                Iterator<Item = Result<<#struct_with_generics as ::unipipe::UniPipe>::Input, TError>> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TIterator
            where
                TIterator: Iterator<Item = Result<<#struct_path_with_generics as ::unipipe::UniPipe>::Input, TError>>,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = Result<<#struct_with_generics as ::unipipe::UniPipe>::Output, TError>> {
                use ::unipipe::UniPipe as _;

                let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);
                let mut completed = false;

                std::iter::from_fn(move || {
                    if completed {
                        return None;
                    }

                    while let Some(input) = self.next() {
                        match input {
                            Ok(input) => {
                                if let Some(output) = pipe.next(Some(input)) {
                                    return Some(Ok(output));
                                }
                            }
                            Err(error) => return Some(Err(error)),
                        }
                    }

                    completed = true;

                    if let Some(output) = pipe.next(None) {
                        return Some(Ok(output));
                    }

                    None
                })
            }
        }
    }
}

struct StreamExtension;

impl Extension for StreamExtension {
    fn get_name() -> &'static str {
        "Stream"
    }

    fn get_pipe_method_name_prefix() -> &'static str {
        ""
    }

    fn get_extra_trait_params() -> Vec<proc_macro2::TokenStream> {
        vec![]
    }

    fn get_extra_impl_for_params() -> Vec<proc_macro2::TokenStream> {
        vec![quote! { TStream }]
    }

    fn generate_trait(
        visibility: &Visibility,
        trait_name: &Ident,
        trait_generics: &ImplGenerics,
        trait_ty_generics: &TypeGenerics,
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                ::unipipe::Stream<Item = <#struct_with_generics as ::unipipe::UniPipe>::Input> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TStream
            where
                TStream: ::unipipe::Stream<Item = <#struct_path_with_generics as ::unipipe::UniPipe>::Input>,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl ::unipipe::Stream<Item = <#struct_with_generics as ::unipipe::UniPipe>::Output> + Unpin {
                use ::unipipe::{StreamExt as _, UniPipe as _};

                Box::pin(::unipipe::stream!({
                    let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);

                    let mut source = Box::pin(self);

                    while let Some(input) = source.next().await {
                        if let Some(output) = pipe.next(Some(input)) {
                            yield output;
                        }
                    }

                    if let Some(output) = pipe.next(None) {
                        yield output;
                    }
                }))
            }
        }
    }
}

struct TryStreamExtension;

impl Extension for TryStreamExtension {
    fn get_name() -> &'static str {
        "TryStream"
    }

    fn get_pipe_method_name_prefix() -> &'static str {
        "try_"
    }

    fn get_extra_trait_params() -> Vec<proc_macro2::TokenStream> {
        vec![quote! { TError }]
    }

    fn get_extra_impl_for_params() -> Vec<proc_macro2::TokenStream> {
        vec![quote! { TStream }]
    }

    fn generate_trait(
        visibility: &Visibility,
        trait_name: &Ident,
        trait_generics: &ImplGenerics,
        trait_ty_generics: &TypeGenerics,
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                ::unipipe::Stream<Item = Result<<#struct_with_generics as ::unipipe::UniPipe>::Input, TError>> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TStream
            where
                TStream: ::unipipe::Stream<Item = Result<<#struct_path_with_generics as ::unipipe::UniPipe>::Input, TError>>,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl ::unipipe::Stream<Item = Result<<#struct_with_generics as ::unipipe::UniPipe>::Output, TError>> + Unpin {
                use ::unipipe::{StreamExt as _, UniPipe as _};

                Box::pin(::unipipe::stream!({
                    let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);

                    let mut source = Box::pin(self);

                    while let Some(input) = source.next().await {
                        match input {
                            Ok(input) => {
                                if let Some(output) = pipe.next(Some(input)) {
                                    yield Ok(output);
                                }
                            }
                            Err(error) => yield Err(error),
                        }
                    }

                    if let Some(output) = pipe.next(None) {
                        yield Ok(output);
                    }
                }))
            }
        }
    }
}

fn method_name_to_pipe_method(method_name: &Ident, struct_name: &Ident) -> Ident {
    let method_str = method_name.to_string();
    let struct_snake = struct_name.to_string().to_case(Case::Snake);

    // If method is "new" or "default", use struct name
    if method_str == "new" || method_str == "default" {
        return format_ident!("{}", struct_snake);
    }

    // If method starts with "new_", replace "new_" with struct name + "_"
    if method_str.starts_with("new_") {
        let suffix = &method_str[4..]; // Remove "new_" prefix
        return format_ident!("{}_{}", struct_snake, suffix);
    }

    // Otherwise, use method name as-is but converted to snake_case
    format_ident!("{}", method_str.to_case(Case::Snake))
}
