use unipipe::{UniPipe, unipipe};

use crate::ChunkWhile;

type ChunkByChunkWhile<'a, TItem> = ChunkWhile<TItem, Box<dyn Fn(&[TItem], &TItem) -> bool + 'a>>;

pub struct ChunkBy<'a, TItem> {
    chunk_while: ChunkByChunkWhile<'a, TItem>,
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl<'a, TItem> ChunkBy<'a, TItem> {
    pub fn new<TIdentifier>(identifier_callback: impl Fn(&TItem) -> TIdentifier + 'a) -> Self
    where
        TIdentifier: PartialEq,
    {
        Self {
            chunk_while: ChunkWhile::new(Box::new(move |chunk, item| {
                identifier_callback(&chunk[0]) == identifier_callback(item)
            })),
        }
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
