use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    FnArg, GenericParam, Generics, Ident, ImplGenerics, ImplItem, ImplItemFn, ItemImpl, Pat,
    PatType, Token, Type, TypeGenerics, Visibility, WhereClause, WherePredicate,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::Comma,
};

// Custom struct to parse attribute arguments
struct UniPipeArgs {
    visibility: Option<Visibility>,
    extensions: Vec<Ident>,
}

impl Parse for UniPipeArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Check if first token is a visibility modifier
        let visibility = if input.peek(Token![pub]) || input.peek(Token![crate]) {
            Some(input.parse()?)
        } else {
            None
        };

        // Parse extension types
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

    // Extract the struct name and generics from the impl block
    let (struct_name, struct_generics) = match &input.self_ty.as_ref() {
        Type::Path(type_path) => {
            let segment = type_path
                .path
                .segments
                .last()
                .expect("Expected struct name");
            (segment.ident.clone(), &input.generics)
        }
        _ => panic!("Expected a simple struct name in impl"),
    };

    // Extract constructor methods from impl block
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

    // To handle where clauses properly, we need to be careful about how we output the impl block
    // The issue is that Rust's parser sees the where clause before macro expansion,
    // and if we output it again, it causes a "duplicate where clause" error.
    // We'll use the parsed input directly.
    let mut output = quote! { #input };

    // Generate extensions for each requested type
    for extension_type in args.extensions {
        let extension = match extension_type.to_string().as_str() {
            "iterator" => IteratorExtension::generate(
                &visibility,
                &struct_name,
                struct_generics,
                &constructor_methods,
            ),
            "try_iterator" => TryIteratorExtension::generate(
                &visibility,
                &struct_name,
                struct_generics,
                &constructor_methods,
            ),
            "stream" => StreamExtension::generate(
                &visibility,
                &struct_name,
                struct_generics,
                &constructor_methods,
            ),
            "try_stream" => TryStreamExtension::generate(
                &visibility,
                &struct_name,
                struct_generics,
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

    fn generate_trait(
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        visibility: &Visibility,
        trait_name: &Ident,
        impl_generics: &ImplGenerics,
        impl_params: &[proc_macro2::TokenStream],
        trait_params_with_bounds: &[&GenericParam],
        trait_param_names: &[proc_macro2::TokenStream],
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream;

    fn generate_trait_method(
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        struct_path: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream;

    fn generate(
        visibility: &Visibility,
        struct_name: &Ident,
        struct_generics: &Generics,
        constructor_methods: &[&ImplItemFn],
    ) -> proc_macro2::TokenStream {
        let trait_name = format_ident!("{}UniPipe{}Ext", struct_name, Self::get_name());

        let (impl_generics, ty_generics, where_clause) = struct_generics.split_for_impl();

        let struct_path = if struct_generics.params.is_empty() {
            quote! { #struct_name }
        } else {
            quote! { #struct_name::#ty_generics }
        };

        // Build proper generic parameters for impl block with correct ordering (lifetimes first)
        let mut impl_params = Vec::new();

        // Add lifetimes first
        for param in &struct_generics.params {
            if let syn::GenericParam::Lifetime(_) = param {
                impl_params.push(quote! { #param });
            }
        }

        // Add type and const parameters
        for param in &struct_generics.params {
            if !matches!(param, syn::GenericParam::Lifetime(_)) {
                impl_params.push(quote! { #param });
            }
        }

        let mut trait_methods = Vec::new();

        for method in constructor_methods {
            let method_name = &method.sig.ident;

            // Extract method arguments (excluding &self, &mut self, self)
            let args: Vec<_> = method
                .sig
                .inputs
                .iter()
                .filter_map(|arg| match arg {
                    FnArg::Typed(pat_type) => Some(pat_type),
                    FnArg::Receiver(_) => None,
                })
                .collect();

            let arg_names: Vec<_> = args.iter().map(|arg| arg.pat.as_ref()).collect();

            trait_methods.push(Self::generate_trait_method(
                &args,
                &arg_names,
                &struct_name,
                &ty_generics,
                &struct_path,
                &method_name,
            ));
        }

        // Extract parameter names and bounds for trait definition
        let trait_params_with_bounds = struct_generics.params.iter().collect::<Vec<_>>();

        // Extract just parameter names (without bounds) for trait reference
        let trait_param_names: Vec<_> = struct_generics
            .params
            .iter()
            .map(|param| match param {
                syn::GenericParam::Type(type_param) => {
                    let ident = &type_param.ident;
                    quote! { #ident }
                }
                syn::GenericParam::Lifetime(lifetime_param) => {
                    let lifetime = &lifetime_param.lifetime;
                    quote! { #lifetime }
                }
                syn::GenericParam::Const(const_param) => {
                    let ident = &const_param.ident;
                    quote! { #ident }
                }
            })
            .collect();

        let where_clause_predicates = where_clause.map(|clause| &clause.predicates);

        Self::generate_trait(
            struct_name,
            &ty_generics,
            visibility,
            &trait_name,
            &impl_generics,
            &impl_params,
            &trait_params_with_bounds,
            &trait_param_names,
            where_clause,
            where_clause_predicates,
            trait_methods,
        )
    }
}

struct IteratorExtension;

impl Extension for IteratorExtension {
    fn get_name() -> &'static str {
        "Iterator"
    }

    fn generate_trait(
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        visibility: &Visibility,
        trait_name: &Ident,
        impl_generics: &ImplGenerics,
        impl_params: &[proc_macro2::TokenStream],
        _trait_params_with_bounds: &[&GenericParam],
        _trait_param_names: &[proc_macro2::TokenStream],
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #impl_generics:
                Iterator<Item = <#struct_name #ty_generics as ::unipipe::UniPipe>::Input> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl<TIterator, #(#impl_params),*> #trait_name #ty_generics for TIterator
            where
                TIterator: Iterator<Item = <#struct_name #ty_generics as ::unipipe::UniPipe>::Input>,
                #struct_name #ty_generics: ::unipipe::UniPipe,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        struct_path: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        let pipe_method_name = method_name_to_pipe_method(method_name, struct_name);

        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = <#struct_name #ty_generics as ::unipipe::UniPipe>::Output> {
                use ::unipipe::UniPipe as _;

                let mut pipe = #struct_path::#method_name(#(#arg_names),*);
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

    fn generate_trait(
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        visibility: &Visibility,
        trait_name: &Ident,
        _impl_generics: &ImplGenerics,
        impl_params: &[proc_macro2::TokenStream],
        trait_params_with_bounds: &[&GenericParam],
        trait_param_names: &[proc_macro2::TokenStream],
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name<TError, #(#trait_params_with_bounds),*>:
                Iterator<Item = Result<<#struct_name #ty_generics as ::unipipe::UniPipe>::Input, TError>> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl<TIterator, TError, #(#impl_params),*> #trait_name<TError, #(#trait_param_names),*> for TIterator
            where
                TIterator: Iterator<Item = Result<<#struct_name #ty_generics as ::unipipe::UniPipe>::Input, TError>>,
                #struct_name #ty_generics: ::unipipe::UniPipe,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        struct_path: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        let pipe_method_name = format_ident!(
            "try_{}",
            method_name_to_pipe_method(method_name, struct_name)
        );

        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = Result<<#struct_name #ty_generics as ::unipipe::UniPipe>::Output, TError>> {
                use ::unipipe::UniPipe as _;

                let mut pipe = #struct_path::#method_name(#(#arg_names),*);
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

    fn generate_trait(
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        visibility: &Visibility,
        trait_name: &Ident,
        impl_generics: &ImplGenerics,
        impl_params: &[proc_macro2::TokenStream],
        _trait_params_with_bounds: &[&GenericParam],
        _trait_param_names: &[proc_macro2::TokenStream],
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name #impl_generics:
                futures::Stream<Item = <#struct_name #ty_generics as ::unipipe::UniPipe>::Input> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl<TStream, #(#impl_params),*> #trait_name #ty_generics for TStream
            where
                TStream: futures::Stream<Item = <#struct_name #ty_generics as ::unipipe::UniPipe>::Input>,
                #struct_name #ty_generics: ::unipipe::UniPipe,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        struct_path: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        let pipe_method_name = method_name_to_pipe_method(method_name, struct_name);

        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl futures::Stream<Item = <#struct_name #ty_generics as ::unipipe::UniPipe>::Output> {
                use futures::StreamExt as _;
                use ::unipipe::UniPipe as _;

                async_stream::stream!({
                    let mut pipe = #struct_path::#method_name(#(#arg_names),*);

                    let mut source = Box::pin(self);

                    while let Some(input) = source.next().await {
                        if let Some(output) = pipe.next(Some(input)) {
                            yield output;
                        }
                    }

                    if let Some(output) = pipe.next(None) {
                        yield output;
                    }
                })
            }
        }
    }
}

struct TryStreamExtension;

impl Extension for TryStreamExtension {
    fn get_name() -> &'static str {
        "TryStream"
    }

    fn generate_trait(
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        visibility: &Visibility,
        trait_name: &Ident,
        _impl_generics: &ImplGenerics,
        impl_params: &[proc_macro2::TokenStream],
        trait_params_with_bounds: &[&GenericParam],
        trait_param_names: &[proc_macro2::TokenStream],
        where_clause: Option<&WhereClause>,
        where_clause_predicates: Option<&Punctuated<WherePredicate, Comma>>,
        methods: Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        quote! {
            #visibility trait #trait_name<TError, #(#trait_params_with_bounds),*>:
                futures::Stream<Item = Result<<#struct_name #ty_generics as ::unipipe::UniPipe>::Input, TError>> + Sized
            #where_clause
            {
                #(#methods)*
            }

            impl<TStream, TError, #(#impl_params),*> #trait_name<TError, #(#trait_param_names),*> for TStream
            where
                TStream: futures::Stream<Item = Result<<#struct_name #ty_generics as ::unipipe::UniPipe>::Input, TError>>,
                #struct_name #ty_generics: ::unipipe::UniPipe,
                #where_clause_predicates
            {}
        }
    }

    fn generate_trait_method(
        args: &[&PatType],
        arg_names: &[&Pat],
        struct_name: &Ident,
        ty_generics: &TypeGenerics,
        struct_path: &proc_macro2::TokenStream,
        method_name: &Ident,
    ) -> proc_macro2::TokenStream {
        let pipe_method_name = format_ident!(
            "try_{}",
            method_name_to_pipe_method(method_name, struct_name)
        );

        quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl futures::Stream<Item = Result<<#struct_name #ty_generics as ::unipipe::UniPipe>::Output, TError>> {
                use futures::StreamExt as _;
                use ::unipipe::UniPipe as _;

                async_stream::stream!({
                    let mut pipe = #struct_path::#method_name(#(#arg_names),*);

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
                })
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
