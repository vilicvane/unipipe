use unipipe::{UniPipe, unipipe};

trait WeightedMovingAverage {}

struct ExponentialMovingAverage {}

impl WeightedMovingAverage for ExponentialMovingAverage {}

struct SimpleMovingAverage {}

impl WeightedMovingAverage for SimpleMovingAverage {}

struct SideTradingPriceDifference<TWeightedMovingAverage> {
    _average: TWeightedMovingAverage,
}

impl<TWeightedMovingAverage: WeightedMovingAverage> UniPipe
    for SideTradingPriceDifference<TWeightedMovingAverage>
{
    type Input = (f64, f64);
    type Output = f64;

    #[allow(refining_impl_trait)]
    fn next(&mut self, _input: Option<Self::Input>) -> Option<Self::Output> {
        unimplemented!()
    }
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl SideTradingPriceDifference<ExponentialMovingAverage> {
    pub fn exponential_moving_average(_volume_window: f64) -> Self {
        Self {
            _average: ExponentialMovingAverage {},
        }
    }
}

#[unipipe(iterator, try_iterator, stream, try_stream)]
impl SideTradingPriceDifference<SimpleMovingAverage> {
    pub fn simple_moving_average(_volume_window: f64) -> Self {
        Self {
            _average: SimpleMovingAverage {},
        }
    }
}
