pub use async_stream::stream;
pub use futures::{Stream, StreamExt};

pub use unipipe_macros::*;

pub trait UniPipe {
    type Input;
    type Output;

    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output>;
}

pub enum Output<T> {
    None,
    One(T),
    Many(Vec<T>),
    End,
}

impl<T> From<Option<T>> for Output<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            None => Self::None,
            Some(value) => Self::One(value),
        }
    }
}
