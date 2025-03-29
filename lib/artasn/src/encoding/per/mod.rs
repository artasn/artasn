mod io;

use std::io::Write;

pub use io::*;

mod encode;
pub use encode::*;

use crate::compiler::Context;

pub(crate) struct PerEncoder<'a, W: Write> {
    pub context: &'a Context,
    pub writer: BitWriter<W>,
    pub tmp_buf: Vec<u8>,
}

#[cfg(target_arch = "wasm32")]
pub(crate) const TMP_BUF_CAPACITY: usize = 1024;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) const TMP_BUF_CAPACITY: usize = 4096;
