# UniPipe

A simple Rust pipe abstraction for that extends to iterator and stream.

```rust
#[derive(Default)]
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

extend_iterator!(pub MyPipe);
extend_try_iterator!(pub MyPipe);

extend_stream!(pub MyPipe);
extend_try_stream!(pub MyPipe);
```
