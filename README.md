# UniPipe

![Crates.io License](https://img.shields.io/crates/l/unipipe?style=flat-square)
![Crates.io Version](https://img.shields.io/crates/v/unipipe?style=flat-square)

A simple Rust pipe abstraction that extends to iterator and stream.

## Installation

```sh
cargo add unipipe
```

## Usage

```rust
use unipipe::{UniPipe, unipipe};

pub struct MyPipe {
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
            // Returning `None` does not indicate the end of the pipe,
            // use `Output::Done` instead.
            None
        }
    }
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl MyPipe {
    // - my_pipe()
    // - try_my_pipe()
    pub fn new() -> Self {
        Self { state: 0 }
    }

    // - my_pipe_with_state(state)
    // - try_my_pipe_with_state(state)
    pub fn new_with_state(state: u32) -> Self {
        Self { state }
    }

    // - custom_pipe()
    // - try_custom_pipe()
    pub fn custom_pipe() -> Self {
        Self { state: 0 }
    }
}
```

To use generated methods from other files, you need to import the generated traits accordingly.

```rust
use my_pipe::MyPipeUniPipeIteratorExt as _;

let mut iter = vec![1, 2, 3, 4, 5].into_iter().my_pipe();
```

### Output

`Output` is the "final" return type of `UniPipe::next()`. It can be one of the following:

- `Output::Next` - equivalent to `Option::None`
- `Output::One(value)` - equivalent to `Option::Some(value)`
- `Output::Many(values)`
- `Output::Done`
- `Output::DoneWithOne(value)`
- `Output::DoneWithMany(values)`

#### Map `Output`

`Output::map()` is a method that allows you to transform the value of the `Output` without changing the structure of the `Output`.

```rust
impl UniPipe for OuterPipe {
    type Input = i32;
    type Output = i32;

    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        self.inner.next(input).map(|output| -output)
    }
}
```

#### Pipe `Output`

`Output::pipe()` is a method that allows you to pipe the `Output` to another `UniPipe`.

```rust
impl UniPipe for ComposePipe {
    type Input = i32;
    type Output = i32;

    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        self.first
            .next(input)
            .pipe(&mut self.second)
            .pipe(&mut self.third)
    }
}
```

## License

This project is licensed under the MIT License.
