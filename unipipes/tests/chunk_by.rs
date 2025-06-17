use unipipes::ChunkByUniPipeIteratorExt as _;

#[test]
fn test_chunk_by() {
    let inputs = vec![1, 1, 2, 2, 2, 3, 3, 4, 5];

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .chunk_by(|item| *item % 2)
                .collect::<Vec<_>>()
        },
        vec![vec![1, 1], vec![2, 2, 2], vec![3, 3], vec![4], vec![5]]
    );
}
