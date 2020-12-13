use crate::gc::{Gc, Mark};
use std::cell::RefCell;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

mod gc;
mod identity;

static A: AtomicU64 = AtomicU64::new(0);

enum GcTree {
    Leaf,
    Knot(Gc<GcTree>, Gc<GcTree>, u8),
}

impl Mark for GcTree {
    fn mark_all(&self) {
        if let GcTree::Knot(a, b, c) = self {
            a.mark_all();
            b.mark_all();
            c.mark_all();
        }
    }
}

impl GcTree {
    pub fn new(depth: u32) -> GcTree {
        A.fetch_add(1, Ordering::Relaxed);
        if depth == 0 {
            return GcTree::Leaf;
        }
        return GcTree::Knot(
            Gc::new(GcTree::new(depth - 1)),
            Gc::new(GcTree::new(depth - 1)),
            10,
        );
    }
}

impl Drop for GcTree {
    fn drop(&mut self) {
        A.fetch_sub(1, Ordering::Relaxed);
    }
}

fn main() {
    println!("Creating tree...");
    let mut before = std::time::Instant::now();
    let tree = Gc::new(GcTree::new(21));
    let tree2 = tree.clone();
    let mut after = std::time::Instant::now();
    println!(
        "Done after {}ms. Running gc...",
        (after - before).as_millis()
    );

    before = std::time::Instant::now();
    Gc::collect();
    after = std::time::Instant::now();

    println!(
        "Done with gc scan after {}ms. Releasing Objects...",
        (after - before).as_millis()
    );

    println!("Created objects: {}", A.load(Ordering::Relaxed));
    std::mem::drop(tree);
    before = std::time::Instant::now();
    Gc::collect();
    std::mem::drop(tree2);
    //Gc::collect();
    after = std::time::Instant::now();

    println!(
        "Done with gc cleanup after {}ms.",
        (after - before).as_millis()
    );

    println!("Done. Remaining objects: {}", A.load(Ordering::Relaxed));
}
