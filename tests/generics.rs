use futures::{StreamExt as _, TryStreamExt as _};

pub struct TestPipe<T> {
    value: Option<T>,
}

impl<T: Clone> unipipe::UniPipe for TestPipe<T> {
    type Input = T;
    type Output = T;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        if let Some(input) = input {
            let output = self.value.clone();
            self.value = Some(input);
            output
        } else {
            self.value.take()
        }
    }
}

#[unipipe::unipipe(iterator, try_iterator)]
impl<T: Clone> TestPipe<T> {
    pub fn new() -> Self {
        Self { value: None }
    }
}

pub struct LifetimeTestPipe<'a, T> {
    _value: Option<&'a T>,
    count: usize,
}

impl<'a, T: Clone> unipipe::UniPipe for LifetimeTestPipe<'a, T> {
    type Input = T;
    type Output = usize;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        if input.is_some() {
            self.count += 1;
            Some(self.count)
        } else {
            None
        }
    }
}

#[unipipe::unipipe(stream, try_stream)]
impl<'a, T> LifetimeTestPipe<'a, Vec<T>>
where
    T: Clone + 'a,
{
    pub fn new_2() -> Self {
        Self {
            _value: None,
            count: 0,
        }
    }
}

#[tokio::test]
async fn test_generic_pipe_1() {
    let inputs = vec![1, 2, 3];
    let outputs = vec![1, 2, 3];

    assert_eq!(
        inputs.clone().into_iter().test_pipe().collect::<Vec<_>>(),
        outputs
    );

    let outputs = Ok(outputs);

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .map(Result::<_, ()>::Ok)
            .try_test_pipe()
            .collect::<Result<Vec<_>, _>>(),
        outputs
    );
}

#[tokio::test]
async fn test_generic_pipe_2() {
    let inputs = vec![vec![1], vec![2], vec![3]];
    let outputs = vec![1, 2, 3];

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .lifetime_test_pipe_2()
            .collect::<Vec<_>>()
            .await,
        outputs
    );

    let outputs = Ok(outputs);

    assert_eq!(
        futures::stream::iter(inputs.clone().into_iter().map(Result::<_, ()>::Ok))
            .try_lifetime_test_pipe_2()
            .try_collect()
            .await,
        outputs
    );
}
