use std::{mem, sync::Mutex};

use unipipe::{Output, UniPipe, unipipe};

pub struct ChunkWhile<TItem, TPredicate>
where
    TPredicate: FnMut(&mut [TItem], &TItem) -> bool,
{
    chunk: Mutex<Vec<TItem>>,
    predicate: TPredicate,
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl<TItem, TPredicate> ChunkWhile<TItem, TPredicate>
where
    TPredicate: FnMut(&mut [TItem], &TItem) -> bool,
{
    pub fn new(predicate: TPredicate) -> Self {
        Self {
            chunk: Mutex::new(Vec::new()),
            predicate,
        }
    }
}

impl<TItem, TPredicate> UniPipe for ChunkWhile<TItem, TPredicate>
where
    TPredicate: FnMut(&mut [TItem], &TItem) -> bool,
{
    type Input = TItem;
    type Output = Vec<TItem>;

    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        let mut chunk = self.chunk.lock().unwrap();

        if let Some(input) = input {
            if chunk.is_empty() || (self.predicate)(&mut chunk, &input) {
                chunk.push(input);

                Output::Next
            } else {
                let output = mem::take(&mut *chunk);

                chunk.push(input);

                Output::One(output)
            }
        } else {
            if chunk.is_empty() {
                Output::Done
            } else {
                Output::DoneWithOne(mem::take(&mut *chunk))
            }
        }
    }
}
