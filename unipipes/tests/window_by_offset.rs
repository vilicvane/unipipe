use unipipes::{WindowByOffsetOptions, WindowByOffsetUniPipeIteratorExt as _};

#[test]
fn test_window_by_offset() {
    let inputs = vec![1, 2, 3, 5, 6, 9, 10, 17];

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .enumerate()
                .window_by_offset(3, 2, |(offset, _)| *offset)
                .map(|(window, range)| (window.into_iter().map(|(_, item)| item).collect(), range))
                .collect::<Vec<_>>()
        },
        vec![
            (vec![1, 2, 3], (0, 3)),
            (vec![3, 5, 6], (2, 5)),
            (vec![6, 9, 10], (4, 7)),
            (vec![10, 17], (6, 9)),
        ]
    );

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .window_by_offset(3, 2, |value| *value)
                .collect::<Vec<_>>()
        },
        vec![
            (vec![1, 2, 3], (1, 4)),
            (vec![3, 5], (3, 6)),
            (vec![5, 6], (5, 8)),
            (vec![9], (7, 10)),
            (vec![9, 10], (9, 12)),
            (vec![], (11, 14)),
            (vec![], (13, 16)),
            (vec![17], (15, 18)),
        ]
    );

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .window_by_offset_with_options(
                    3,
                    2,
                    |value| *value,
                    WindowByOffsetOptions {
                        ignore_empty_windows: true,
                        ..Default::default()
                    },
                )
                .collect::<Vec<_>>()
        },
        vec![
            (vec![1, 2, 3], (1, 4)),
            (vec![3, 5], (3, 6)),
            (vec![5, 6], (5, 8)),
            (vec![9], (7, 10)),
            (vec![9, 10], (9, 12)),
            (vec![17], (15, 18)),
        ]
    );

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .window_by_offset_with_options(
                    3,
                    2,
                    |value| *value,
                    WindowByOffsetOptions {
                        align_window_start: true,
                        ..Default::default()
                    },
                )
                .collect::<Vec<_>>()
        },
        vec![
            (vec![1, 2], (0, 3)),
            (vec![2, 3], (2, 5)),
            (vec![5, 6], (4, 7)),
            (vec![6], (6, 9)),
            (vec![9, 10], (8, 11)),
            (vec![10], (10, 13)),
            (vec![], (12, 15)),
            (vec![], (14, 17)),
            (vec![17], (16, 19)),
        ]
    );

    assert_eq!(
        {
            inputs
                .clone()
                .into_iter()
                .window_by_offset_with_options(
                    3,
                    2,
                    |value| *value,
                    WindowByOffsetOptions {
                        align_window_start: true,
                        ignore_empty_windows: true,
                    },
                )
                .collect::<Vec<_>>()
        },
        vec![
            (vec![1, 2], (0, 3)),
            (vec![2, 3], (2, 5)),
            (vec![5, 6], (4, 7)),
            (vec![6], (6, 9)),
            (vec![9, 10], (8, 11)),
            (vec![10], (10, 13)),
            (vec![17], (16, 19)),
        ]
    );
}
