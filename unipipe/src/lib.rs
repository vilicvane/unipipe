mod iterator;
mod stream;

pub use paste;
pub use unipipe_macros::*;

pub trait UniPipe: Default {
    type Input;
    type Output;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output>;
}
