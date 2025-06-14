use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    FnArg, Ident, ImplItem, ImplItemFn, ItemImpl, Token, Type, Visibility,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
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

    // Extract the struct name from the impl block
    let struct_name = match &input.self_ty.as_ref() {
        Type::Path(type_path) => type_path
            .path
            .segments
            .last()
            .expect("Expected struct name")
            .ident
            .clone(),
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

    let mut output = quote! { #input };

    // Generate extensions for each requested type
    for extension_type in args.extensions {
        let extension = match extension_type.to_string().as_str() {
            "iterator" => {
                generate_iterator_extension(&struct_name, &visibility, &constructor_methods)
            }
            "try_iterator" => {
                generate_try_iterator_extension(&struct_name, &visibility, &constructor_methods)
            }
            "stream" => generate_stream_extension(&struct_name, &visibility, &constructor_methods),
            "try_stream" => {
                generate_try_stream_extension(&struct_name, &visibility, &constructor_methods)
            }
            _ => panic!("Unknown extension type: {}", extension_type),
        };

        output.extend(extension);
    }

    output.into()
}

fn generate_iterator_extension(
    struct_name: &Ident,
    visibility: &Visibility,
    constructor_methods: &[&ImplItemFn],
) -> proc_macro2::TokenStream {
    let trait_name = format_ident!("{}UniPipeIteratorExt", struct_name);

    let mut trait_methods = Vec::new();

    for method in constructor_methods {
        let method_name = &method.sig.ident;
        let pipe_method_name = method_name_to_pipe_method(method_name, struct_name);

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

        let arg_names: Vec<_> = args.iter().map(|arg| &arg.pat).collect();

        trait_methods.push(quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = <#struct_name as ::unipipe::UniPipe>::Output> {
                let mut pipe = #struct_name::#method_name(#(#arg_names),*);
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
        });
    }

    quote! {
        #visibility trait #trait_name:
            Iterator<Item = <#struct_name as ::unipipe::UniPipe>::Input> + Sized
        {
            #(#trait_methods)*
        }

        impl<TIterator> #trait_name for TIterator where
            TIterator: Iterator<Item = <#struct_name as ::unipipe::UniPipe>::Input>
        {}
    }
}

fn generate_try_iterator_extension(
    struct_name: &Ident,
    visibility: &Visibility,
    constructor_methods: &[&ImplItemFn],
) -> proc_macro2::TokenStream {
    let trait_name = format_ident!("{}UniPipeIteratorTryExt", struct_name);

    let mut trait_methods = Vec::new();

    for method in constructor_methods {
        let method_name = &method.sig.ident;
        let pipe_method_name = format_ident!(
            "try_{}",
            method_name_to_pipe_method(method_name, struct_name)
        );

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

        let arg_names: Vec<_> = args.iter().map(|arg| &arg.pat).collect();

        trait_methods.push(quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl Iterator<Item = Result<<#struct_name as ::unipipe::UniPipe>::Output, TError>> {
                let mut pipe = #struct_name::#method_name(#(#arg_names),*);
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
        });
    }

    quote! {
        #visibility trait #trait_name<TError>:
            Iterator<Item = Result<<#struct_name as ::unipipe::UniPipe>::Input, TError>> + Sized
        {
            #(#trait_methods)*
        }

        impl<TIterator, TError> #trait_name<TError> for TIterator where
            TIterator: Iterator<Item = Result<<#struct_name as ::unipipe::UniPipe>::Input, TError>>
        {}
    }
}

fn generate_stream_extension(
    struct_name: &Ident,
    visibility: &Visibility,
    constructor_methods: &[&ImplItemFn],
) -> proc_macro2::TokenStream {
    let trait_name = format_ident!("{}UniPipeStreamExt", struct_name);

    let mut trait_methods = Vec::new();

    for method in constructor_methods {
        let method_name = &method.sig.ident;
        let pipe_method_name = method_name_to_pipe_method(method_name, struct_name);

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

        let arg_names: Vec<_> = args.iter().map(|arg| &arg.pat).collect();

        trait_methods.push(quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl futures::Stream<Item = <#struct_name as ::unipipe::UniPipe>::Output> {
                use futures::StreamExt as _;

                async_stream::stream!({
                    let mut pipe = #struct_name::#method_name(#(#arg_names),*);

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
        });
    }

    quote! {
        #visibility trait #trait_name:
            futures::Stream<Item = <#struct_name as ::unipipe::UniPipe>::Input> + Sized
        {
            #(#trait_methods)*
        }

        impl<TStream> #trait_name for TStream where
            TStream: futures::Stream<Item = <#struct_name as ::unipipe::UniPipe>::Input>
        {}
    }
}

fn generate_try_stream_extension(
    struct_name: &Ident,
    visibility: &Visibility,
    constructor_methods: &[&ImplItemFn],
) -> proc_macro2::TokenStream {
    let trait_name = format_ident!("{}UniPipeTryStreamExt", struct_name);

    let mut trait_methods = Vec::new();

    for method in constructor_methods {
        let method_name = &method.sig.ident;
        let pipe_method_name = format_ident!(
            "try_{}",
            method_name_to_pipe_method(method_name, struct_name)
        );

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

        let arg_names: Vec<_> = args.iter().map(|arg| &arg.pat).collect();

        trait_methods.push(quote! {
            fn #pipe_method_name(
                mut self,
                #(#args),*
            ) -> impl futures::Stream<Item = Result<<#struct_name as ::unipipe::UniPipe>::Output, TError>> {
                use futures::StreamExt as _;

                async_stream::stream!({
                    let mut pipe = #struct_name::#method_name(#(#arg_names),*);

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
        });
    }

    quote! {
        #visibility trait #trait_name<TError>:
            futures::Stream<Item = Result<<#struct_name as ::unipipe::UniPipe>::Input, TError>> + Sized
        {
            #(#trait_methods)*
        }

        impl<TStream, TError> #trait_name<TError> for TStream where
            TStream: futures::Stream<Item = Result<<#struct_name as ::unipipe::UniPipe>::Input, TError>>
        {}
    }
}

fn method_name_to_pipe_method(method_name: &Ident, struct_name: &Ident) -> Ident {
    let method_str = method_name.to_string();
    let struct_snake = struct_name.to_string().to_case(Case::Snake);

    // If method is "new", use struct name
    if method_str == "new" {
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
