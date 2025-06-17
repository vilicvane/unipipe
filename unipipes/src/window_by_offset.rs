use std::{
    collections::VecDeque,
    mem,
    ops::{Add, Rem, Sub},
};

use unipipe::{Output, UniPipe, unipipe};

pub struct WindowByOffset<TItem, TOffset, TDistance, TOffsetCallback> {
    window_size: TDistance,
    window_step: TDistance,
    offset_callback: TOffsetCallback,
    align_window_start: bool,
    ignore_empty_windows: bool,
    window: VecDeque<TItem>,
    window_start: Option<TOffset>,
}

#[derive(Default)]
pub struct WindowByOffsetOptions {
    /// If `true`, the starting point of each window will be aligned to the
    /// nearest multiple of `window_step` (i.e., windows start at 0,
    /// window_step, 2 * window_step, etc.). If `false`, windows will start from
    /// the offset of the first item.
    pub align_window_start: bool,
    pub ignore_empty_windows: bool,
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl<TItem, TOffset, TDistance, TOffsetCallback>
    WindowByOffset<TItem, TOffset, TDistance, TOffsetCallback>
where
    TItem: Clone,
    TOffsetCallback: FnMut(&TItem) -> TOffset,
    TOffset: Add<TDistance, Output = TOffset>
        + Sub<TDistance, Output = TOffset>
        + Sub<TOffset, Output = TDistance>
        + Rem<TDistance, Output = TDistance>
        + PartialOrd
        + Copy,
    TDistance: Rem<TDistance, Output = TDistance> + Copy,
{
    pub fn new(
        window_size: TDistance,
        window_step: TDistance,
        offset_callback: TOffsetCallback,
    ) -> Self {
        Self::new_with_options(
            window_size,
            window_step,
            offset_callback,
            WindowByOffsetOptions::default(),
        )
    }

    pub fn new_with_options(
        window_size: TDistance,
        window_step: TDistance,
        offset_callback: TOffsetCallback,
        WindowByOffsetOptions {
            align_window_start,
            ignore_empty_windows,
        }: WindowByOffsetOptions,
    ) -> Self {
        Self {
            window_size,
            window_step,
            offset_callback,
            align_window_start,
            ignore_empty_windows,
            window: VecDeque::new(),
            window_start: None,
        }
    }
}

impl<TItem, TOffset, TDistance, TOffsetCallback> UniPipe
    for WindowByOffset<TItem, TOffset, TDistance, TOffsetCallback>
where
    TItem: Clone,
    TOffsetCallback: FnMut(&TItem) -> TOffset,
    TOffset: Add<TDistance, Output = TOffset>
        + Sub<TDistance, Output = TOffset>
        + Sub<TOffset, Output = TDistance>
        + Rem<TDistance, Output = TDistance>
        + PartialOrd
        + Copy,
    TDistance: Rem<TDistance, Output = TDistance> + Copy,
{
    type Input = TItem;
    type Output = (Vec<TItem>, (TOffset, TOffset));

    #[allow(refining_impl_trait)]
    fn next(&mut self, input: Option<Self::Input>) -> Output<Self::Output> {
        if let Some(item) = input {
            let item_offset = (self.offset_callback)(&item);

            let Some(mut window_start) = self.window_start else {
                let window_start = if self.align_window_start {
                    item_offset - item_offset % self.window_step
                } else {
                    item_offset
                };

                self.window_start = Some(window_start);
                self.window.push_back(item);

                return Output::Next;
            };

            let mut outputs = Vec::new();

            loop {
                let window_end = window_start + self.window_size;

                if window_end > item_offset {
                    // Current window overlaps with the next item, so we'll need
                    // to wait more items before we can output it.
                    break;
                }

                // Otherwise, no more items will ever be added to the window
                // (and possibly the next one), until the next window overlaps
                // with input item.

                // Example:
                // .....      .
                //            ^ current
                // *****         < output 1
                //    **---      < output 2
                //       -----   < output 3
                //          --*  < pending

                let window = self.window.iter().cloned().collect::<Vec<_>>();

                if self.ignore_empty_windows && window.is_empty() {
                    // As shown in the example above, the pending window could
                    // be empty now. We could simply skip it but imagine a
                    // sparse sequence of items, moving the window step by step
                    // is not ideal. We would want to calculate the next
                    // window_start and jump there directly.

                    window_start = item_offset - (item_offset - window_end) % self.window_step
                        + self.window_step
                        - self.window_size;

                    self.window_start = Some(window_start);

                    // After jumping to the new window start, the current window
                    // will need to wait for more items. So we just break the
                    // loop and push the current item to the window (after loop
                    // block).

                    break;
                }

                outputs.push((window, (window_start, window_end)));

                // Now we move the window one `step` forward, and remove items
                // before the new window start.

                window_start = window_start + self.window_step;

                self.window_start = Some(window_start);

                let obsolete_end = self
                    .window
                    .partition_point(|item| (self.offset_callback)(item) < window_start);

                self.window.drain(..obsolete_end);
            }

            self.window.push_back(item);

            return Output::Many(outputs);
        }

        // The source sequence ended, let's do some cleanup.

        if self.window.is_empty() {
            Output::Done
        } else {
            let window = mem::take(&mut self.window).into();

            let window_start = self.window_start.unwrap();

            Output::DoneWithOne((window, (window_start, window_start + self.window_size)))
        }
    }
}
