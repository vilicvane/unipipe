use std::collections::VecDeque;

use unipipe::{Output, UniPipe};

#[derive(Default)]
pub struct MyPipe {}

impl UniPipe for MyPipe {
    type Input = String;
    type Output = usize;

    fn next(&mut self, input: Option<Self::Input>) -> impl Into<Output<Self::Output>> {
        if let Some(input) = input {
            if !input.is_empty() {
                return Some(input.len());
            }
        }

        None
    }
}

pub trait MyPipeUniPipeIteratorExt: Iterator<Item = <MyPipe as UniPipe>::Input> + Sized {
    fn my_pipe(mut self) -> impl Iterator<Item = <MyPipe as UniPipe>::Output> {
        let mut pipe = <MyPipe as Default>::default();

        let mut pending = VecDeque::new();
        let mut done = false;

        std::iter::from_fn(move || {
            if let Some(output) = pending.pop_front() {
                return Some(output);
            }

            loop {
                if done {
                    return None;
                }

                let input = self.next();

                if input.is_none() {
                    done = true;
                }

                let next_output: Output<_> = pipe.next(input).into();

                if next_output.is_done() {
                    done = true;
                }

                match next_output {
                    Output::One(output) | Output::DoneWithOne(output) => {
                        return Some(output);
                    }
                    Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                        let mut outputs = outputs.into_iter();

                        if let Some(output) = outputs.next() {
                            pending.extend(outputs);
                            return Some(output);
                        }
                    }
                    Output::Next | Output::Done => {}
                }
            }
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

        let mut pending = VecDeque::new();
        let mut done = false;

        std::iter::from_fn(move || {
            if let Some(output) = pending.pop_front() {
                return Some(Ok(output));
            }

            loop {
                if done {
                    return None;
                }

                let input = self.next();

                if input.is_none() {
                    done = true;
                }

                let input = match input {
                    Some(Err(error)) => return Some(Err(error)),
                    Some(Ok(input)) => Some(input),
                    None => None,
                };

                let next_output: Output<_> = pipe.next(input).into();

                if next_output.is_done() {
                    done = true;
                }

                match next_output {
                    Output::One(output) | Output::DoneWithOne(output) => return Some(Ok(output)),
                    Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                        let mut outputs = outputs.into_iter();

                        if let Some(output) = outputs.next() {
                            pending.extend(outputs);
                            return Some(Ok(output));
                        }
                    }
                    Output::Next | Output::Done => {}
                }
            }
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
            let mut done = false;

            loop {
                if done {
                    break;
                }

                let input = source.next().await;

                if input.is_none() {
                    done = true;
                }

                let next_output: Output<_> = pipe.next(input).into();

                if next_output.is_done() {
                    done = true;
                }

                match next_output {
                    Output::One(output) | Output::DoneWithOne(output) => yield output,
                    Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                        for output in outputs {
                            yield output;
                        }
                    }
                    Output::Next | Output::Done => {}
                }
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
            let mut done = false;

            loop {
                if done {
                    break;
                }

                let input = source.next().await;

                if input.is_none() {
                    done = true;
                }

                let input = match input {
                    Some(Err(error)) => {
                        yield Err(error);
                        continue;
                    }
                    Some(Ok(input)) => Some(input),
                    None => None,
                };

                let next_output: Output<_> = pipe.next(input).into();

                if next_output.is_done() {
                    done = true;
                }

                match next_output {
                    Output::One(output) | Output::DoneWithOne(output) => yield Ok(output),
                    Output::Many(outputs) | Output::DoneWithMany(outputs) => {
                        for output in outputs {
                            yield Ok(output);
                        }
                    }
                    Output::Next | Output::Done => {}
                }
            }
        })
    }
}

impl<TStream, TError> MyPipeUniPipeTryStreamExt<TError> for TStream where
    TStream: futures::Stream<Item = Result<<MyPipe as UniPipe>::Input, TError>>
{
}

fn main() {}
