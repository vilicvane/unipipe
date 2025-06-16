use std::mem;

use unipipe::{Output, UniPipe, unipipe};

struct InnerPipe {}

impl UniPipe for InnerPipe {
    type Input = i32;
    type Output = i32;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        match input {
            None => Output::Next,
            Some(0) => Output::Done,
            Some(1) => Output::One(1),
            Some(2) => Output::Many(vec![2, 2]),
            Some(3) => Output::DoneWithOne(3),
            Some(4) => Output::DoneWithMany(vec![4, 4]),
            _ => unreachable!(),
        }
    }
}

struct OuterPipe {
    inner: InnerPipe,
}

impl UniPipe for OuterPipe {
    type Input = i32;
    type Output = i32;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        self.inner.next(input).map(|output| -output)
    }
}

#[unipipe(iterator)]
impl OuterPipe {
    pub fn new() -> Self {
        Self {
            inner: InnerPipe {},
        }
    }
}

#[tokio::test]
async fn test_compose_1() {
    assert_eq!(
        vec![1, 2].into_iter().outer_pipe().collect::<Vec<_>>(),
        vec![-1, -2, -2]
    );

    assert_eq!(
        vec![1, 3].into_iter().outer_pipe().collect::<Vec<_>>(),
        vec![-1, -3]
    );

    assert_eq!(
        vec![2, 4].into_iter().outer_pipe().collect::<Vec<_>>(),
        vec![-2, -2, -4, -4]
    );

    assert_eq!(
        vec![1, 2, 0, 3]
            .into_iter()
            .outer_pipe()
            .collect::<Vec<_>>(),
        vec![-1, -2, -2]
    );
}

struct SlicePipe {
    size: usize,
    pending: Vec<i32>,
}

impl UniPipe for SlicePipe {
    type Input = i32;
    type Output = Vec<i32>;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        if let Some(input) = input {
            self.pending.push(input);

            if self.pending.len() >= self.size {
                Output::One(mem::take(&mut self.pending))
            } else {
                Output::Next
            }
        } else {
            Output::One(mem::take(&mut self.pending))
        }
    }
}

struct ComposePipe {
    first: InnerPipe,
    second: SlicePipe,
}

impl UniPipe for ComposePipe {
    type Input = i32;
    type Output = Vec<i32>;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        self.first.next(input).pipe(&mut self.second)
    }
}

#[unipipe(iterator)]
impl ComposePipe {
    pub fn new() -> Self {
        Self {
            first: InnerPipe {},
            second: SlicePipe {
                size: 2,
                pending: vec![],
            },
        }
    }
}

#[tokio::test]
async fn test_compose_2() {
    assert_eq!(
        vec![1, 2, 3].into_iter().compose_pipe().collect::<Vec<_>>(),
        vec![vec![1, 2], vec![2, 3]]
    );

    assert_eq!(
        vec![1, 2, 4].into_iter().compose_pipe().collect::<Vec<_>>(),
        vec![vec![1, 2], vec![2, 4], vec![4]]
    );

    assert_eq!(
        vec![1, 2, 0].into_iter().compose_pipe().collect::<Vec<_>>(),
        vec![vec![1, 2], vec![2]]
    );
}
