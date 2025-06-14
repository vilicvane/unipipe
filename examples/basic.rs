use futures::{StreamExt as _, TryStreamExt as _};

use unipipe::{UniPipe, extend_iterator, extend_stream, extend_try_iterator, extend_try_stream};

#[derive(Default)]
struct MyPipe {
    state: u32,
}

impl UniPipe for MyPipe {
    type Input = u32;
    type Output = u32;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        if let Some(input) = input {
            self.state += input;
            Some(self.state)
        } else {
            None
        }
    }
}

extend_iterator!(MyPipe);
extend_try_iterator!(MyPipe);

extend_stream!(MyPipe);
extend_try_stream!(MyPipe);

#[tokio::main]
async fn main() {
    let inputs = vec![1, 2, 3, 4, 5];
    let outputs = vec![1, 3, 6, 10, 15];

    assert_eq!(
        inputs.clone().into_iter().my_pipe().collect::<Vec<_>>(),
        outputs
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .my_pipe()
            .collect::<Vec<_>>()
            .await,
        outputs
    );

    let inputs = inputs
        .into_iter()
        .map(Result::<_, std::fmt::Error>::Ok)
        .chain(vec![Err(std::fmt::Error::default())])
        .collect::<Vec<_>>();

    let outputs = Err(std::fmt::Error::default());

    assert_eq!(
        inputs
            .clone()
            .into_iter()
            .try_my_pipe()
            .collect::<Result<Vec<_>, _>>(),
        outputs
    );

    assert_eq!(
        futures::stream::iter(inputs.clone())
            .try_my_pipe()
            .try_collect()
            .await,
        outputs
    );
}
