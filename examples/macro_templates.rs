use unipipe::UniPipe;

#[derive(Default)]
pub struct MyPipe {}

impl UniPipe for MyPipe {
    type Input = String;
    type Output = usize;

    fn next(&mut self, input: Option<Self::Input>) -> Option<Self::Output> {
        if let Some(input) = input {
            if input.len() > 0 {
                return Some(input.len());
            }
        }

        None
    }
}

pub trait MyPipeUniPipeIteratorExt: Iterator<Item = <MyPipe as UniPipe>::Input> + Sized {
    fn my_pipe(mut self) -> impl Iterator<Item = <MyPipe as UniPipe>::Output> {
        let mut pipe = <MyPipe as Default>::default();
        let mut completed = false;

        std::iter::from_fn(move || {
            if completed {
                return None;
            }

            while let Some(input) = self.next() {
                if let Some(output) = pipe.next(Some(input)) {
                    return Some(output);
                }
            }

            completed = true;

            if let Some(output) = pipe.next(None) {
                return Some(output);
            }

            None
        })
    }
}

impl<TIterator> MyPipeUniPipeIteratorExt for TIterator where
    TIterator: Iterator<Item = <MyPipe as UniPipe>::Input>
{
}

pub trait MyPipeUniPipeIteratorTryExt<TError>:
    Iterator<Item = Result<<MyPipe as UniPipe>::Input, TError>> + Sized
{
    fn try_my_pipe(mut self) -> impl Iterator<Item = Result<<MyPipe as UniPipe>::Output, TError>> {
        let mut pipe = <MyPipe as Default>::default();
        let mut completed = false;

        std::iter::from_fn(move || {
            if completed {
                return None;
            }

            while let Some(input) = self.next() {
                match input {
                    Ok(input) => {
                        if let Some(output) = pipe.next(Some(input)) {
                            return Some(Ok(output));
                        }
                    }
                    Err(error) => return Some(Err(error)),
                }
            }

            completed = true;

            if let Some(output) = pipe.next(None) {
                return Some(Ok(output));
            }

            None
        })
    }
}

impl<TIterator, TError> MyPipeUniPipeIteratorTryExt<TError> for TIterator where
    TIterator: Iterator<Item = Result<<MyPipe as UniPipe>::Input, TError>>
{
}

pub trait MyPipeUniPipeStreamExt:
    futures::Stream<Item = <MyPipe as UniPipe>::Input> + Sized
{
    fn my_pipe(self) -> impl futures::Stream<Item = <MyPipe as UniPipe>::Output> {
        use futures::StreamExt as _;

        unipipe::stream!({
            let mut pipe = <MyPipe as Default>::default();

            let mut source = Box::pin(self);

            while let Some(input) = source.next().await {
                if let Some(output) = pipe.next(Some(input)) {
                    yield output;
                }
            }

            if let Some(output) = pipe.next(None) {
                yield output;
            }
        })
    }
}

impl<TStream> MyPipeUniPipeStreamExt for TStream where
    TStream: futures::Stream<Item = <MyPipe as UniPipe>::Input>
{
}

pub trait MyPipeUniPipeTryStreamExt<TError>:
    futures::Stream<Item = Result<<MyPipe as UniPipe>::Input, TError>> + Sized
{
    fn try_my_pipe(
        self,
    ) -> impl futures::Stream<Item = Result<<MyPipe as UniPipe>::Output, TError>> {
        use futures::StreamExt as _;

        unipipe::stream!({
            let mut pipe = <MyPipe as Default>::default();

            let mut source = Box::pin(self);

            while let Some(input) = source.next().await {
                match input {
                    Ok(input) => {
                        if let Some(output) = pipe.next(Some(input)) {
                            yield Ok(output);
                        }
                    }
                    Err(error) => yield Err(error),
                }
            }

            if let Some(output) = pipe.next(None) {
                yield Ok(output);
            }
        })
    }
}

impl<TStream, TError> MyPipeUniPipeTryStreamExt<TError> for TStream where
    TStream: futures::Stream<Item = Result<<MyPipe as UniPipe>::Input, TError>>
{
}

fn main() {}
