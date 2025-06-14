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

#[unipipe::unipipe(stream, try_stream)]
impl<T> TestPipe<T>
where
    T: Clone,
{
    pub fn new_2() -> Self {
        Self { value: None }
    }
}

#[tokio::test]
async fn test_generic_pipe() {
    let inputs = vec![1, 2, 3];
    let outputs = vec![1, 2, 3];

    assert_eq!(
        inputs.clone().into_iter().test_pipe().collect::<Vec<_>>(),
        outputs
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .test_pipe_2()
            .collect::<Vec<_>>()
            .await,
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

    assert_eq!(
        futures::stream::iter(inputs.clone().into_iter().map(Result::<_, ()>::Ok))
            .try_test_pipe_2()
            .try_collect()
            .await,
        outputs
    );
}
