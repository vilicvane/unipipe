pub use async_stream::stream;
pub use futures::{Stream, StreamExt};

pub use unipipe_macros::*;

pub trait UniPipe {
    type Input;
    type Output;

    fn next(&mut self, input: Option<Self::Input>) -> impl Into<Output<Self::Output>>;
}

pub enum Output<T> {
    Next,
    One(T),
    Many(Vec<T>),
    Done,
    DoneWithOne(T),
    DoneWithMany(Vec<T>),
}

impl<T> Output<T> {
    pub fn is_done(&self) -> bool {
        matches!(
            self,
            Self::Done | Self::DoneWithOne(_) | Self::DoneWithMany(_)
        )
    }

    pub fn map<TMapped, TCallback>(self, mut callback: TCallback) -> Output<TMapped>
    where
        TCallback: FnMut(T) -> TMapped,
    {
        match self {
            Self::Next => Output::<TMapped>::Next,
            Self::One(value) => Output::<TMapped>::One(callback(value)),
            Self::Many(values) => {
                Output::<TMapped>::Many(values.into_iter().map(callback).collect())
            }
            Self::Done => Output::<TMapped>::Done,
            Self::DoneWithOne(value) => Output::<TMapped>::DoneWithOne(callback(value)),
            Self::DoneWithMany(values) => {
                Output::<TMapped>::DoneWithMany(values.into_iter().map(callback).collect())
            }
        }
    }

    pub fn pipe<TPipe, TPipeOutput: std::fmt::Debug>(self, pipe: &mut TPipe) -> Output<TPipeOutput>
    where
        TPipe: UniPipe<Input = T, Output = TPipeOutput>,
    {
        let upper_done = self.is_done();

        let output = match self {
            Self::Next => Output::<TPipeOutput>::Next,
            Self::One(value) | Self::DoneWithOne(value) => pipe.next(Some(value)).into(),
            Self::Many(values) | Self::DoneWithMany(values) => {
                let mut aggregated_outputs = Vec::new();

                let mut inputs = values.into_iter().map(Some).collect::<Vec<_>>();

                if upper_done {
                    inputs.push(None);
                }

                for value in inputs {
                    let next_output: Output<_> = pipe.next(value).into();

                    let done = next_output.is_done();

                    match next_output {
                        Output::One(output) | Output::DoneWithOne(output) => {
                            aggregated_outputs.push(output)
                        }
                        Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                            aggregated_outputs.extend(outputs)
                        }
                        Output::Next | Output::Done => {}
                    }

                    if done {
                        return Output::<TPipeOutput>::DoneWithMany(aggregated_outputs);
                    }
                }

                Output::<TPipeOutput>::Many(aggregated_outputs)
            }
            Self::Done => pipe.next(None).into(),
        };

        if upper_done { output.done() } else { output }
    }

    fn done(self) -> Output<T> {
        match self {
            Self::Next => Self::Done,
            Self::One(value) => Self::DoneWithOne(value),
            Self::Many(values) => Self::DoneWithMany(values),
            _ => self,
        }
    }
}

impl<T> From<Option<T>> for Output<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            None => Self::Next,
            Some(value) => Self::One(value),
        }
    }
}
