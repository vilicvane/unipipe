use futures::{StreamExt as _, TryStreamExt as _};
use unipipe::UniPipe;

struct SumFive {
    sum: u32,
}

impl UniPipe for SumFive {
    type Input = u32;
    type Output = u32;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        if let Some(input) = input {
            let sum = self.sum + input;

            if sum >= 5 {
                self.sum = 0;
                Some(sum)
            } else {
                self.sum = sum;
                None
            }
        } else {
            let sum = self.sum;

            if sum > 0 { Some(sum) } else { None }
        }
    }
}

#[unipipe::unipipe(iterator, try_iterator, stream, try_stream)]
impl SumFive {
    pub fn new() -> Self {
        Self { sum: 0 }
    }

    pub fn sum_five_default() -> Self {
        Self { sum: 0 }
    }

    pub fn new_with_initial_sum(sum: u32) -> Self {
        Self { sum }
    }
}

#[tokio::test]
async fn test_values() {
    let inputs = vec![1, 2, 3, 4, 5, 1];
    let outputs = vec![6, 9, 1];

    assert_eq!(
        inputs.clone().into_iter().sum_five().collect::<Vec<_>>(),
        outputs
    );

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .sum_five_default()
            .collect::<Vec<_>>(),
        outputs
    );

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .sum_five_with_initial_sum(1)
            .collect::<Vec<_>>(),
        vec![7, 9, 1]
    );

    assert_eq!(
        futures::stream::iter(inputs)
            .sum_five()
            .collect::<Vec<_>>()
            .await,
        outputs
    );
}

#[tokio::test]
async fn test_results_1() {
    let inputs = vec![1, 2, 3, 4, 5, 1]
        .into_iter()
        .map(Result::<_, std::fmt::Error>::Ok)
        .collect::<Vec<_>>();

    let outputs = Ok(vec![6, 9, 1]);

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .try_sum_five()
            .collect::<Result<Vec<_>, _>>(),
        outputs
    );

    assert_eq!(
        futures::stream::iter(inputs)
            .try_sum_five()
            .try_collect()
            .await,
        outputs
    );
}

#[tokio::test]
async fn test_results_2() {
    let inputs = vec![1, 2, 3, 4, 5]
        .into_iter()
        .map(Result::<_, std::fmt::Error>::Ok)
        .chain(vec![Err(std::fmt::Error::default())])
        .collect::<Vec<_>>();

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .try_sum_five()
            .collect::<Result<Vec<_>, _>>(),
        Err(std::fmt::Error::default())
    );

    assert_eq!(
        futures::stream::iter(inputs)
            .try_sum_five()
            .try_collect::<Vec<_>>()
            .await,
        Err(std::fmt::Error::default())
    );
}
