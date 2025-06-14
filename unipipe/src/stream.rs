#[macro_export]
macro_rules! extend_stream {
    ($visibility:vis $struct_name:ident) => {
        $crate::paste::paste! {
            $crate::extend_stream!($visibility $struct_name, [<$struct_name:snake>]);
        }
    };
    ($visibility:vis $struct_name:ident, $method_name:ident) => {
        $crate::paste::paste! {
            $visibility trait [<$struct_name UniPipeStreamExt>]:
                futures::Stream<Item = <$struct_name as $crate::UniPipe>::Input> + Sized
            {
                fn $method_name(
                    mut self,
                ) -> impl futures::Stream<Item = <$struct_name as $crate::UniPipe>::Output> {
                    use futures::StreamExt as _;

                    async_stream::stream!({
                        let mut pipe = <$struct_name as Default>::default();

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

            impl<TStream> [<$struct_name UniPipeStreamExt>] for TStream where
                TStream: futures::Stream<Item = <$struct_name as $crate::UniPipe>::Input>
            {}
        }
    };
}

#[macro_export]
macro_rules! extend_try_stream {
    ($visibility:vis $struct_name:ident) => {
        $crate::paste::paste! {
            $crate::extend_try_stream!($visibility $struct_name, [<try_ $struct_name:snake>]);
        }
    };
    ($visibility:vis $struct_name:ident, $method_name:ident) => {
        $crate::paste::paste! {
            $visibility trait [<$struct_name UniPipeTryStreamExt>]<TError>:
                futures::Stream<Item = Result<<$struct_name as $crate::UniPipe>::Input, TError>> + Sized
            {
                fn $method_name(mut self) -> impl futures::Stream<Item = Result<<$struct_name as $crate::UniPipe>::Output, TError>> {
                    use futures::StreamExt as _;

                    async_stream::stream!({
                        let mut pipe = <$struct_name as Default>::default();

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

            impl<TStream, TError> [<$struct_name UniPipeTryStreamExt>]<TError> for TStream where
                TStream: futures::Stream<Item = Result<<$struct_name as $crate::UniPipe>::Input, TError>>
            {}
        }
    };
}
