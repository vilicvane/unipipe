use futures::StreamExt as _;
use unipipe::{Output, UniPipe};

struct AdvancedOutput1 {
    end: bool,
}

impl UniPipe for AdvancedOutput1 {
    type Input = usize;
    type Output = usize;

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        if let Some(input) = input {
            Output::Many((1..=input).collect())
        } else {
            if self.end {
                Output::One(0)
            } else {
                Output::Next
            }
        }
    }
}

#[unipipe::unipipe(iterator, try_iterator, stream, try_stream)]
impl AdvancedOutput1 {
    pub fn new() -> Self {
        Self { end: false }
    }

    pub fn new_end() -> Self {
        Self { end: true }
    }
}

#[tokio::test]
async fn test_advanced_output_1() {
    let inputs = vec![1, 2, 3];

    let outputs_end_true = vec![1, 1, 2, 1, 2, 3, 0];
    let outputs_end_false = vec![1, 1, 2, 1, 2, 3];

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .advanced_output_1()
            .collect::<Vec<_>>(),
        outputs_end_false
    );

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .advanced_output_1_end()
            .collect::<Vec<_>>(),
        outputs_end_true
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .advanced_output_1()
            .collect::<Vec<_>>()
            .await,
        outputs_end_false
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .advanced_output_1_end()
            .collect::<Vec<_>>()
            .await,
        outputs_end_true
    );

    let mut input = inputs.into_iter().map(Ok::<_, ()>).collect::<Vec<_>>();

    input.insert(3, Err(()));

    let mut outputs_end_true = outputs_end_true.into_iter().map(Ok).collect::<Vec<_>>();
    let mut outputs_end_false = outputs_end_false.into_iter().map(Ok).collect::<Vec<_>>();

    outputs_end_true.insert(6, Err(()));
    outputs_end_false.insert(6, Err(()));

    assert_eq!(
        input
            .clone()
            .into_iter()
            .try_advanced_output_1()
            .collect::<Vec<_>>(),
        outputs_end_false
    );

    assert_eq!(
        input
            .clone()
            .into_iter()
            .try_advanced_output_1_end()
            .collect::<Vec<_>>(),
        outputs_end_true
    );

    assert_eq!(
        futures::stream::iter(input.clone())
            .try_advanced_output_1()
            .collect::<Vec<_>>()
            .await,
        outputs_end_false
    );

    assert_eq!(
        futures::stream::iter(input.clone())
            .try_advanced_output_1_end()
            .collect::<Vec<_>>()
            .await,
        outputs_end_true
    );
}

struct AdvancedOutput2 {
    limit: usize,
}

impl UniPipe for AdvancedOutput2 {
    type Input = usize;
    type Output = usize;

    fn next(&mut self, input: Option<Self::Input>) -> impl Into<Output<Self::Output>> {
        if let Some(input) = input {
            if input > self.limit {
                Output::Done
            } else {
                Output::One(input)
            }
        } else {
            Output::Next
        }
    }
}

#[unipipe::unipipe(iterator, try_iterator, stream, try_stream)]
impl AdvancedOutput2 {
    pub fn new(limit: usize) -> Self {
        Self { limit }
    }
}

#[tokio::test]
async fn test_advanced_output_2() {
    let inputs = (1..=5).collect::<Vec<_>>();

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .advanced_output_2(3)
            .collect::<Vec<_>>(),
        inputs.clone().into_iter().take(3).collect::<Vec<_>>()
    );

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .advanced_output_2(10)
            .collect::<Vec<_>>(),
        inputs
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .advanced_output_2(3)
            .collect::<Vec<_>>()
            .await,
        inputs.clone().into_iter().take(3).collect::<Vec<_>>()
    );

    let mut inputs = inputs.into_iter().map(Ok::<_, ()>).collect::<Vec<_>>();

    inputs[2] = Err(());

    let mut outputs = (1..=5).map(Ok).collect::<Vec<_>>();

    outputs[2] = Err(());

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .try_advanced_output_2(3)
            .collect::<Vec<_>>(),
        outputs.clone().into_iter().take(3).collect::<Vec<_>>()
    );

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .try_advanced_output_2(10)
            .collect::<Vec<_>>(),
        outputs
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .try_advanced_output_2(3)
            .collect::<Vec<_>>()
            .await,
        outputs.clone().into_iter().take(3).collect::<Vec<_>>()
    );
}
