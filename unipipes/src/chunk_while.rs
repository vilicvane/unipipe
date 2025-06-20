use std::mem;

use unipipe::{UniPipe, unipipe};

pub struct ChunkWhile<TItem, TPredicate>
where
    TPredicate: FnMut(&[TItem], &TItem) -> bool,
{
    chunk: Vec<TItem>,
    predicate: TPredicate,
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl<TItem, TPredicate> ChunkWhile<TItem, TPredicate>
where
    TPredicate: FnMut(&[TItem], &TItem) -> bool,
{
    pub fn new(predicate: TPredicate) -> Self {
        Self {
            chunk: Vec::new(),
            predicate,
        }
    }
}

impl<TItem, TPredicate> UniPipe for ChunkWhile<TItem, TPredicate>
where
    TPredicate: FnMut(&[TItem], &TItem) -> bool,
{
    type Input = TItem;
    type Output = Vec<TItem>;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        if let Some(input) = input {
            if self.chunk.is_empty() || (self.predicate)(&self.chunk, &input) {
                self.chunk.push(input);

                None
            } else {
                let output = mem::take(&mut self.chunk);

                self.chunk.push(input);

                Some(output)
            }
        } else {
            if self.chunk.is_empty() {
                None
            } else {
                Some(mem::take(&mut self.chunk))
            }
        }
    }
}
