use unipipe::{UniPipe, unipipe};

use crate::ChunkWhile;

pub struct ChunkBy<'a, TItem> {
    chunk_while: ChunkWhile<TItem, Box<dyn FnMut(&mut [TItem], &TItem) -> bool + 'a>>,
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl<'a, TItem> ChunkBy<'a, TItem> {
    pub fn new<TIdentifier>(mut identifier_callback: impl FnMut(&TItem) -> TIdentifier + 'a) -> Self
    where
        TIdentifier: PartialEq,
    {
        Self {
            chunk_while: ChunkWhile::new(Box::new(move |chunk, item| {
                identifier_callback(&chunk[0]) == identifier_callback(item)
            })),
        }
    }

    /// Avoid conflict with the `chunk_by` method from the `itertools` crate.
    pub fn unipipe_chunk_by<TIdentifier>(
        identifier_callback: impl FnMut(&TItem) -> TIdentifier + 'a,
    ) -> Self
    where
        TIdentifier: PartialEq,
    {
        Self::new(identifier_callback)
    }
}

impl<'a, TItem> UniPipe for ChunkBy<'a, TItem> {
    type Input = TItem;
    type Output = Vec<TItem>;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        self.chunk_while.next(input)
    }
}
