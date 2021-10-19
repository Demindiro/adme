#![cfg_attr(feature = "jit", feature(asm, naked_functions))]

mod assembler;
mod interpreter;
#[cfg(feature = "jit")]
mod jit;
mod memory;
mod util;

pub use assembler::{Assembler, SourceMap};
pub use interpreter::Cpu;
pub use memory::*;

#[cfg(feature = "wasm")]
mod wasm;
