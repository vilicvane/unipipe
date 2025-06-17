use std::{
    collections::VecDeque,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll},
};

use futures::Stream;
use unipipe::{Output, UniPipe};

#[derive(Default)]
pub struct MyPipe {}

impl MyPipe {
    fn new() -> Self {
        Self::default()
    }

    fn set_some_option(&mut self) {}
}

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

pub struct MyPipeUniPipeIterator<TIterator> {
    pipe: Arc<Mutex<MyPipe>>,
    iterator: TIterator,
}

impl<TIterator> MyPipeUniPipeIterator<TIterator> {
    fn set_some_option(self) -> Self {
        MyPipe::set_some_option(&mut self.pipe.lock().unwrap());
        self
    }
}

impl<TIterator> Iterator for MyPipeUniPipeIterator<TIterator>
where
    TIterator: Iterator<Item = <MyPipe as UniPipe>::Output>,
{
    type Item = <MyPipe as UniPipe>::Output;

    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next()
    }
}

pub trait MyPipeUniPipeIteratorExt: Iterator<Item = <MyPipe as UniPipe>::Input> + Sized {
    fn my_pipe(
        mut self,
    ) -> MyPipeUniPipeIterator<impl Iterator<Item = <MyPipe as UniPipe>::Output>> {
        let pipe = Arc::new(Mutex::new(<MyPipe as Default>::default()));

        let mut pending = VecDeque::new();
        let mut done = false;

        MyPipeUniPipeIterator {
            pipe: pipe.clone(),
            iterator: std::iter::from_fn(move || {
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

                    let next_output: Output<_> = pipe.lock().unwrap().next(input).into();

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
            }),
        }
    }
}

impl<TIterator> MyPipeUniPipeIteratorExt for TIterator where
    TIterator: Iterator<Item = <MyPipe as UniPipe>::Input>
{
}

pub struct MyPipeUniPipeIteratorTry<T> {
    inner: T,
}

impl<TIterator, TError> Iterator for MyPipeUniPipeIteratorTry<TIterator>
where
    TIterator: Iterator<Item = Result<<MyPipe as UniPipe>::Output, TError>>,
{
    type Item = Result<<MyPipe as UniPipe>::Output, TError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub trait MyPipeUniPipeIteratorTryExt<TError>:
    Iterator<Item = Result<<MyPipe as UniPipe>::Input, TError>> + Sized
{
    fn try_my_pipe(
        mut self,
    ) -> MyPipeUniPipeIteratorTry<impl Iterator<Item = Result<<MyPipe as UniPipe>::Output, TError>>>
    {
        let mut pipe = <MyPipe as Default>::default();

        let mut pending = VecDeque::new();
        let mut done = false;

        let iterator = std::iter::from_fn(move || {
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
        });

        MyPipeUniPipeIteratorTry { inner: iterator }
    }
}

impl<TIterator, TError> MyPipeUniPipeIteratorTryExt<TError> for TIterator where
    TIterator: Iterator<Item = Result<<MyPipe as UniPipe>::Input, TError>>
{
}

pub struct MyPipeUniPipeStream<T> {
    inner: T,
}

impl<TStream> Stream for MyPipeUniPipeStream<TStream>
where
    TStream: futures::Stream<Item = <MyPipe as UniPipe>::Output> + Unpin,
{
    type Item = <MyPipe as UniPipe>::Output;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Pin::new(&mut self.inner).poll_next(cx)
    }
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

pub struct MyPipeUniPipeTryStream<T> {
    inner: T,
}

impl<TStream, TError> Stream for MyPipeUniPipeTryStream<TStream>
where
    TStream: futures::Stream<Item = Result<<MyPipe as UniPipe>::Output, TError>> + Unpin,
{
    type Item = Result<<MyPipe as UniPipe>::Output, TError>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Pin::new(&mut self.inner).poll_next(cx)
    }
}

pub trait MyPipeUniPipeTryStreamExt<TError>:
    futures::Stream<Item = Result<<MyPipe as UniPipe>::Input, TError>> + Sized
{
    fn try_my_pipe(
        self,
    ) -> MyPipeUniPipeTryStream<
        impl futures::Stream<Item = Result<<MyPipe as UniPipe>::Output, TError>>,
    > {
        use futures::StreamExt as _;

        let stream = unipipe::stream!({
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
        });

        MyPipeUniPipeTryStream { inner: stream }
    }
}

impl<TStream, TError> MyPipeUniPipeTryStreamExt<TError> for TStream where
    TStream: futures::Stream<Item = Result<<MyPipe as UniPipe>::Input, TError>>
{
}

fn main() {
    let inputs = vec!["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
        .into_iter()
        .map(|s| s.to_owned())
        .collect::<Vec<_>>();

    let outputs = inputs.into_iter().my_pipe().collect::<Vec<_>>();

    assert_eq!(outputs, vec![1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
}
