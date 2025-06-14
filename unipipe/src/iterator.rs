#[macro_export]
macro_rules! extend_iterator {
    ($visibility:vis $struct_name:ident) => {
        $crate::paste::paste! {
            $crate::extend_iterator!($visibility $struct_name, [<$struct_name:snake>]);
        }
    };
    ($visibility:vis $struct_name:ident, $method_name:ident) => {
        $crate::paste::paste! {
            $visibility trait [<$struct_name UniPipeIteratorExt>]:
                Iterator<Item = <$struct_name as $crate::UniPipe>::Input> + Sized
            {
                fn $method_name(
                    mut self,
                ) -> impl Iterator<Item = <$struct_name as $crate::UniPipe>::Output> {
                    let mut pipe = <$struct_name as Default>::default();
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

            impl<TIterator> [<$struct_name UniPipeIteratorExt>] for TIterator where
                TIterator: Iterator<Item = <$struct_name as $crate::UniPipe>::Input>
            {}
        }
    };
}

#[macro_export]
macro_rules! extend_try_iterator {
    ($visibility:vis $struct_name:ident) => {
        $crate::paste::paste! {
            $crate::extend_try_iterator!($visibility $struct_name, [<try_ $struct_name:snake>]);
        }
    };
    ($visibility:vis $struct_name:ident, $method_name:ident) => {
        $crate::paste::paste! {
            $visibility trait [<$struct_name UniPipeIteratorTryExt>]<TError>:
                Iterator<Item = Result<<$struct_name as $crate::UniPipe>::Input, TError>> + Sized
            {
                fn $method_name(mut self) -> impl Iterator<Item = Result<<$struct_name as $crate::UniPipe>::Output, TError>> {
                    let mut pipe = <$struct_name as Default>::default();
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

            impl<TIterator, TError> [<$struct_name UniPipeIteratorTryExt>]<TError> for TIterator where
                TIterator: Iterator<Item = Result<<$struct_name as $crate::UniPipe>::Input, TError>>
            {}
        }
    };
}
