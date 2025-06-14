# UniPipe

A simple Rust pipe abstraction that extends to iterator and stream.

## Installation

```sh
cargo add unipipe

# For stream support:
cargo add futures async-stream
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
            None
        }
    }
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl MyPipe {
    // -> my_pipe()
    pub fn new() -> Self {
        Self { state: 0 }
    }

    // -> my_pipe_with_state(state)
    pub fn new_with_state(state: u32) -> Self {
        Self { state }
    }

    // -> custom_pipe()
    pub fn custom_pipe() -> Self {
        Self { state: 0 }
    }
}
```
