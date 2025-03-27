mod io;

pub use io::*;

mod encode;
pub use encode::*;

const MAX_VLQ_LEN: usize = 10;
