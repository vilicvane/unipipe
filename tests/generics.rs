use futures::{StreamExt as _, TryStreamExt as _};
use unipipe::Output;

pub struct TestPipe<T> {
    value: Option<T>,
}

impl<T: Clone> unipipe::UniPipe for TestPipe<T> {
    type Input = T;
    type Output = T;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        if let Some(input) = input {
            let output = self.value.clone();
            self.value = Some(input);
            output
        } else {
            self.value.take()
        }
        .into()
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

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        if input.is_some() {
            self.count += 1;
            Some(self.count)
        } else {
            None
        }
        .into()
    }
}

#[unipipe::unipipe(stream, try_stream)]
impl<'a, T> LifetimeTestPipe<'a, Vec<T>>
where
    T: Clone + 'a,
{
    pub fn new_2<P>(_p: P) -> Self
    where
        P: PartialEq,
    {
        Self {
            _value: None,
            count: 0,
        }
    }
}

pub struct LifetimeTestPipe2<'a, T> {
    prefix: &'a str,
    transformer: &'a dyn Fn(&T) -> String,
    buffer: Vec<String>,
}

impl<'a, T> unipipe::UniPipe for LifetimeTestPipe2<'a, T> {
    type Input = T;
    type Output = String;

    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        match input {
            Some(value) => {
                let transformed = format!("{}: {}", self.prefix, (self.transformer)(&value));
                self.buffer.push(transformed.clone());
                Some(transformed)
            }
            None => {
                // On completion, return a summary if buffer has items
                if !self.buffer.is_empty() {
                    let summary = format!(
                        "[{}] Processed {} items: {}",
                        self.prefix,
                        self.buffer.len(),
                        self.buffer.join(", ")
                    );
                    self.buffer.clear();
                    Some(summary)
                } else {
                    None
                }
            }
        }
        .into()
    }
}

#[unipipe::unipipe(iterator, try_iterator)]
impl<'a, T> LifetimeTestPipe2<'a, T> {
    pub fn new(prefix: &'a str, transformer: &'a dyn Fn(&T) -> String) -> Self {
        Self {
            prefix,
            transformer,
            buffer: Vec::new(),
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
            .lifetime_test_pipe_2(())
            .collect::<Vec<_>>()
            .await,
        outputs
    );

    let outputs = Ok(outputs);

    assert_eq!(
        futures::stream::iter(inputs.clone().into_iter().map(Result::<_, ()>::Ok))
            .try_lifetime_test_pipe_2(())
            .try_collect()
            .await,
        outputs
    );
}

#[tokio::test]
async fn test_lifetime_pipe_2() {
    // Test with borrowed data that has a specific lifetime
    let prefix = "Item";
    let transformer = |x: &i32| x.to_string();

    let inputs = vec![1, 2, 3];
    let expected_outputs = vec![
        "Item: 1".to_string(),
        "Item: 2".to_string(),
        "Item: 3".to_string(),
        "[Item] Processed 3 items: Item: 1, Item: 2, Item: 3".to_string(),
    ];

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .lifetime_test_pipe_2(prefix, &transformer)
            .collect::<Vec<_>>(),
        expected_outputs
    );

    // Test with try_iterator
    let expected_outputs = Ok(expected_outputs);

    assert_eq!(
        inputs
            .into_iter()
            .map(Result::<_, ()>::Ok)
            .try_lifetime_test_pipe_2(prefix, &transformer)
            .collect::<Result<Vec<_>, _>>(),
        expected_outputs
    );

    // Test with different types and lifetimes
    let string_prefix = "String";
    let string_transformer = |s: &String| format!("len={}", s.len());
    let string_inputs = vec!["hello".to_string(), "world".to_string()];

    let string_outputs: Vec<_> = string_inputs
        .into_iter()
        .lifetime_test_pipe_2(string_prefix, &string_transformer)
        .collect();

    assert_eq!(string_outputs.len(), 3); // 2 items + 1 summary
    assert_eq!(string_outputs[0], "String: len=5");
    assert_eq!(string_outputs[1], "String: len=5");
    assert!(string_outputs[2].starts_with("[String] Processed 2 items:"));
}
