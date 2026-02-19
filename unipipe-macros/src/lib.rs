use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::{
    AngleBracketedGenericArguments, FnArg, GenericParam, Generics, Ident, ImplGenerics, ImplItem,
    ImplItemFn, ItemImpl, LitStr, Pat, PatType, PathArguments, Token, Type, TypeGenerics,
    Visibility, WhereClause, WherePredicate,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::Comma,
};

struct UniPipeArgs {
    name: Option<String>,
    visibility: Option<Visibility>,
    extensions: Vec<Ident>,
}

impl Parse for UniPipeArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = if input.peek(LitStr) {
            let string_literal: LitStr = input.parse()?;

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }

            Some(string_literal.value())
        } else {
            None
        };

        let visibility = if input.peek(Token![pub]) || input.peek(Token![crate]) {
            Some(input.parse()?)
        } else {
            None
        };

        let extensions = Punctuated::<Ident, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect();

        Ok(UniPipeArgs {
            name,
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
        if let ImplItem::Fn(method) = item
            && (method.vis == Visibility::Inherited || matches!(method.vis, Visibility::Public(_)))
        {
            constructor_methods.push(method);
        }
    }

    if constructor_methods.is_empty() {
        panic!("No public constructor methods found in impl block");
    }

    let visibility = args.visibility.unwrap_or_else(|| parse_quote!(pub));

    let mut output = quote! {
        #[allow(clippy::new_without_default)]
        #input
    };

    for extension_type in args.extensions {
        let extension = match extension_type.to_string().as_str() {
            "iterator" => IteratorExtension::generate(
                args.name.as_deref(),
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            "try_iterator" => TryIteratorExtension::generate(
                args.name.as_deref(),
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            "stream" => StreamExtension::generate(
                args.name.as_deref(),
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            "try_stream" => TryStreamExtension::generate(
                args.name.as_deref(),
                &visibility,
                struct_name,
                impl_struct_generics,
                impl_struct_impl_generics,
                &constructor_methods,
            ),
            _ => panic!("Unknown extension type: {extension_type}"),
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
        trait_generics: proc_macro2::TokenStream,
        trait_ty_generics: &TypeGenerics,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        method_signatures: Vec<proc_macro2::TokenStream>,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream;

    fn generate_trait_method_signature(
        pipe_method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
    ) -> proc_macro2::TokenStream;

    fn generate_trait_method(
        pipe_method_name: &Ident,
        method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
        arg_names: &[&Ident],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream;

    fn generate(
        specified_name: Option<&str>,
        visibility: &Visibility,
        struct_name: &Ident,
        impl_struct_generics: Option<&AngleBracketedGenericArguments>,
        impl_struct_impl_generics: &Generics,
        constructor_methods: &[&ImplItemFn],
    ) -> proc_macro2::TokenStream {
        let trait_name = format_ident!(
            "{}UniPipe{}Ext",
            specified_name.unwrap_or(struct_name.to_string().as_str()),
            Self::get_name()
        );

        let where_clause = impl_struct_impl_generics.where_clause.as_ref();

        let (struct_with_generics, struct_path_with_generics) = if impl_struct_generics.is_none() {
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

        // We need to pass the self type down to pick up its implied bounds in the extension trait
        // and impl. Using a default value will avoid needing to repeat it in every trait impl.
        //
        // Unfortunately, this default value isn't retained once the generics go through
        // `split_for_impl`, and the trait generics setup above may be incomplete. To work around
        // this, we thus push this new generic parameter where it's needed (only at the end of the
        // *trait*'s generics list), but it's slightly inefficient.
        let mut trait_generics: Generics = parse_quote! { #trait_generics };
        let self_ty_param: GenericParam = parse_quote! {
            _SELF: ::unipipe::UniPipe = #struct_with_generics
        };
        trait_generics.params.push(self_ty_param);

        let (impl_generics, _, _) = impl_for_generics.split_for_impl();

        let where_clause_predicates = where_clause.map(|clause| &clause.predicates);

        let mut trait_method_signatures = Vec::new();
        let mut trait_method_bodies = Vec::new();

        for method in constructor_methods {
            let args: Vec<_> = method
                .sig
                .inputs
                .iter()
                .enumerate()
                .filter_map(|(index, arg)| match arg {
                    FnArg::Typed(pat_type) => Some(simplify_arg_pat_type(pat_type, index)),
                    FnArg::Receiver(_) => None,
                })
                .collect();

            if args.len() != method.sig.inputs.len() {
                continue;
            }

            let arg_names: Vec<_> = args
                .iter()
                .map(|arg| match &*arg.pat {
                    Pat::Ident(pat_ident) => &pat_ident.ident,
                    _ => panic!("Expected PatIdent in simplified args"),
                })
                .collect();

            let pipe_method_name = format_ident!(
                "{}{}",
                Self::get_pipe_method_name_prefix(),
                method_name_to_pipe_method(&method.sig.ident, struct_name)
            );

            let (method_impl_generics, _, where_clause) = method.sig.generics.split_for_impl();

            trait_method_signatures.push(Self::generate_trait_method_signature(
                &pipe_method_name,
                &method_impl_generics,
                where_clause,
                &args,
            ));
            trait_method_bodies.push(Self::generate_trait_method(
                &pipe_method_name,
                &method.sig.ident,
                &method_impl_generics,
                where_clause,
                &args,
                &arg_names,
                &struct_with_generics,
                &struct_path_with_generics,
            ));
        }

        Self::generate_trait(
            visibility,
            &trait_name,
            trait_generics.to_token_stream(),
            &trait_ty_generics,
            &struct_path_with_generics,
            where_clause,
            where_clause_predicates,
            &impl_generics,
            trait_method_signatures,
            trait_method_bodies,
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
        trait_generics: proc_macro2::TokenStream,
        trait_ty_generics: &TypeGenerics,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        method_signatures: Vec<proc_macro2::TokenStream>,
        method_bodies: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                Iterator<Item = <_SELF as ::unipipe::UniPipe>::Input> + Sized
            #where_clause
            {
                #(#method_signatures)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TIterator
            where
                TIterator: Iterator<Item = <#struct_path_with_generics as ::unipipe::UniPipe>::Input>,
                #where_clause_predicates
            {
                #(#method_bodies)*
            }
        }
    }

    fn generate_trait_method_signature(
        pipe_method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                self,
                #(#args),*
            ) -> impl Iterator<Item = <_SELF as ::unipipe::UniPipe>::Output>
            #where_clause;
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
        arg_names: &[&Ident],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = <#struct_with_generics as ::unipipe::UniPipe>::Output>
            #where_clause {
                use ::unipipe::*;

                let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);

                let mut pending = std::collections::VecDeque::new();
                let mut done = false;

                std::iter::from_fn(move || {
                    if let Some(output) = pending.pop_front() {
                        return Some(output);
                    }

                    loop {
                        if done {
                            return None;
                        }

                        let input = self.next();

                        if input.is_none() {
                            done = true;
                        }

                        let next_output: Output<_> = pipe.next(input).into();

                        if next_output.is_done() {
                            done = true;
                        }

                        match next_output {
                            Output::One(output) | Output::DoneWithOne(output) => return Some(output),
                            Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                                let mut outputs = outputs.into_iter();

                                if let Some(output) = outputs.next() {
                                    pending.extend(outputs);
                                    return Some(output);
                                }
                            }
                            Output::Next | Output::Done => {}
                        }
                    }
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
        trait_generics: proc_macro2::TokenStream,
        trait_ty_generics: &TypeGenerics,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        method_signatures: Vec<proc_macro2::TokenStream>,
        method_bodies: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                Iterator<Item = Result<<_SELF as ::unipipe::UniPipe>::Input, TError>> + Sized
            #where_clause
            {
                #(#method_signatures)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TIterator
            where
                TIterator: Iterator<Item = Result<<#struct_path_with_generics as ::unipipe::UniPipe>::Input, TError>>,
                #where_clause_predicates
            {
                #(#method_bodies)*
            }
        }
    }

    fn generate_trait_method_signature(
        pipe_method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                self,
                #(#args),*
            ) -> impl Iterator<Item = Result<<_SELF as ::unipipe::UniPipe>::Output, TError>>
            #where_clause;
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
        arg_names: &[&Ident],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = Result<<#struct_with_generics as ::unipipe::UniPipe>::Output, TError>>
            #where_clause {
                use ::unipipe::*;

                let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);

                let mut pending = std::collections::VecDeque::new();
                let mut done = false;

                std::iter::from_fn(move || {
                    if let Some(output) = pending.pop_front() {
                        return Some(Ok(output));
                    }

                    loop {
                        if done {
                            return None;
                        }

                        let input = self.next();

                        if input.is_none() {
                            done = true;
                        }

                        let input = match input {
                            Some(Err(error)) => return Some(Err(error)),
                            Some(Ok(input)) => Some(input),
                            None => None,
                        };

                        let next_output: Output<_> = pipe.next(input).into();

                        if next_output.is_done() {
                            done = true;
                        }

                        match next_output {
                            Output::One(output) | Output::DoneWithOne(output) => return Some(Ok(output)),
                            Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                                let mut outputs = outputs.into_iter();

                                if let Some(output) = outputs.next() {
                                    pending.extend(outputs);
                                    return Some(Ok(output));
                                }
                            }
                            Output::Next | Output::Done => {}
                        }
                    }
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
        trait_generics: proc_macro2::TokenStream,
        trait_ty_generics: &TypeGenerics,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        method_signatures: Vec<proc_macro2::TokenStream>,
        method_bodies: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                ::unipipe::Stream<Item = <_SELF as ::unipipe::UniPipe>::Input> + Sized
            #where_clause
            {
                #(#method_signatures)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TStream
            where
                TStream: ::unipipe::Stream<Item = <#struct_path_with_generics as ::unipipe::UniPipe>::Input>,
                #where_clause_predicates
            {
                #(#method_bodies)*
            }
        }
    }

    fn generate_trait_method_signature(
        pipe_method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                self,
                #(#args),*
            ) -> impl ::unipipe::Stream<Item = <_SELF as ::unipipe::UniPipe>::Output> + Unpin
            #where_clause;
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
        arg_names: &[&Ident],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                mut self,
                #(#args),*
            ) -> impl ::unipipe::Stream<Item = <#struct_with_generics as ::unipipe::UniPipe>::Output> + Unpin
            #where_clause {
                use ::unipipe::*;

                Box::pin(::unipipe::stream!({
                    let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);

                    let mut source = Box::pin(self);
                    let mut done = false;

                    loop {
                        if done {
                            break;
                        }

                        let input = source.next().await;

                        if input.is_none() {
                            done = true;
                        }

                        let next_output: Output<_> = pipe.next(input).into();

                        if next_output.is_done() {
                            done = true;
                        }

                        match next_output {
                            Output::One(output) | Output::DoneWithOne(output) => yield output,
                            Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                                for output in outputs {
                                    yield output;
                                }
                            }
                            Output::Next | Output::Done => {}
                        }
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
        trait_generics: proc_macro2::TokenStream,
        trait_ty_generics: &TypeGenerics,
        struct_path_with_generics: &proc_macro2::TokenStream,
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        impl_generics: &ImplGenerics,
        method_signatures: Vec<proc_macro2::TokenStream>,
        method_bodies: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #trait_generics:
                ::unipipe::Stream<Item = Result<<_SELF as ::unipipe::UniPipe>::Input, TError>> + Sized
            #where_clause
            {
                #(#method_signatures)*
            }

            impl #impl_generics #trait_name #trait_ty_generics for TStream
            where
                TStream: ::unipipe::Stream<Item = Result<<#struct_path_with_generics as ::unipipe::UniPipe>::Input, TError>>,
                #where_clause_predicates
            {
                #(#method_bodies)*
            }
        }
    }

    fn generate_trait_method_signature(
        pipe_method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                self,
                #(#args),*
            ) -> impl ::unipipe::Stream<Item = Result<<_SELF as ::unipipe::UniPipe>::Output, TError>> + Unpin
            #where_clause;
        }
    }

    fn generate_trait_method(
        pipe_method_name: &Ident,
        method_name: &Ident,
        method_impl_generics: &ImplGenerics,
        where_clause: Option<&WhereClause>,
        args: &[PatType],
        arg_names: &[&Ident],
        struct_with_generics: &proc_macro2::TokenStream,
        struct_path_with_generics: &proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        quote! {
            fn #pipe_method_name #method_impl_generics(
                mut self,
                #(#args),*
            ) -> impl ::unipipe::Stream<Item = Result<<#struct_with_generics as ::unipipe::UniPipe>::Output, TError>> + Unpin
            #where_clause {
                use ::unipipe::*;

                Box::pin(::unipipe::stream!({
                    let mut pipe = #struct_path_with_generics::#method_name(#(#arg_names),*);

                    let mut source = Box::pin(self);
                    let mut done = false;

                    loop {
                        if done {
                            break;
                        }

                        let input = source.next().await;

                        if input.is_none() {
                            done = true;
                        }

                        let input = match input {
                            Some(Err(error)) => {
                                yield Err(error);
                                continue;
                            }
                            Some(Ok(input)) => Some(input),
                            None => None,
                        };

                        let next_output: Output<_> = pipe.next(input).into();

                        if next_output.is_done() {
                            done = true;
                        }

                        match next_output {
                            Output::One(output) | Output::DoneWithOne(output) => yield Ok(output),
                            Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                                for output in outputs {
                                    yield Ok(output);
                                }
                            }
                            Output::Next | Output::Done => {}
                        }
                    }
                }))
            }
        }
    }
}

fn method_name_to_pipe_method(method_name: &Ident, struct_name: &Ident) -> Ident {
    let method_str = method_name.to_string();
    let struct_snake = struct_name.to_string().to_case(Case::Snake);

    if method_str == "new" {
        return format_ident!("{}", struct_snake);
    }

    if let Some(suffix) = method_str.strip_prefix("new_") {
        return format_ident!("{}_{}", struct_snake, suffix);
    }

    format_ident!("{}", method_str.to_case(Case::Snake))
}

fn simplify_arg_pat_type(pat_type: &PatType, index: usize) -> PatType {
    let mut simplified = pat_type.clone();

    let ident = match &*pat_type.pat {
        Pat::Ident(pat_ident) => pat_ident.ident.clone(),
        _ => format_ident!("arg_{}", index),
    };

    simplified.pat = Box::new(Pat::Ident(syn::PatIdent {
        attrs: vec![],
        by_ref: None,
        mutability: None,
        ident,
        subpat: None,
    }));

    simplified
}
