use unipipes::ChunkWhileUniPipeIteratorExt as _;

#[test]
fn test_chunk_while() {
    let inputs = vec![1, 2, 3, 5, 6, 9, 10, 17];

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .chunk_while(|chunk, item| chunk.last().unwrap() + 1 == *item)
                .collect::<Vec<_>>()
        },
        vec![vec![1, 2, 3], vec![5, 6], vec![9, 10], vec![17]]
    );
}
