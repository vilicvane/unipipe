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
        match self {
            Self::Done | Self::DoneWithOne(_) | Self::DoneWithMany(_) => true,
            _ => false,
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
