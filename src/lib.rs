#![feature(drain_filter)]
mod gc;
pub use gc::*;
pub use safe_gc_derive::Mark;
