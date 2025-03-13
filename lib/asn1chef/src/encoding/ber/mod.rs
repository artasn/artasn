mod encode;
pub use encode::*;

mod reader;

mod decode;
pub use decode::*;

const MAX_VLQ_LEN: usize = 10;
