pub use async_stream::stream;
pub use futures::{Stream, StreamExt};

pub use unipipe_macros::*;

pub trait UniPipe {
    type Input;
    type Output;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output>;
}
