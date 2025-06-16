pub use async_stream::stream;
pub use futures::{Stream, StreamExt};

pub use unipipe_macros::*;

pub trait UniPipe {
    type Input;
    type Output;

    fn next(&mut self, input: Option<Self::Input>) -> impl Into<Output<Self::Output>>;
}

pub enum Output<T> {
    One(T),
    Many(Vec<T>),
    Next,
    End,
}

impl<T> From<Option<T>> for Output<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            None => Self::Next,
            Some(value) => Self::One(value),
        }
    }
}
