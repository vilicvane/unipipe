use futures::{StreamExt as _, TryStreamExt as _};
use unipipe::{UniPipe, extend_iterator, extend_stream, extend_try_iterator, extend_try_stream};

#[derive(Default)]
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

extend_iterator!(SumFive);
extend_try_iterator!(SumFive);

extend_stream!(SumFive);
extend_try_stream!(SumFive);

#[tokio::test]
async fn test_values() {
    let inputs = vec![1, 2, 3, 4, 5, 1];
    let outputs = vec![6, 9, 1];

    assert_eq!(
        inputs.clone().into_iter().sum_five().collect::<Vec<_>>(),
        outputs
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
