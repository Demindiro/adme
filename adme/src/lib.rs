#![cfg_attr(feature = "jit", feature(asm, naked_functions, new_uninit))]

mod assembler;
mod interpreter;
#[cfg(feature = "jit")]
mod jit;
mod memory;
mod util;

pub use assembler::{Assembler, SourceMap};
pub use interpreter::Cpu;
#[cfg(feature = "jit")]
pub use jit::{Jit, Registers};
pub use memory::*;

#[cfg(feature = "wasm")]
mod wasm;
