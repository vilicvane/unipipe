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

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
