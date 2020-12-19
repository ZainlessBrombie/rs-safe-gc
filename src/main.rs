#![feature(drain_filter)]

use gc::{Finalize, Gc, GcCell, Trace};
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::atomic::{AtomicU64, Ordering};

//mod gc;

static A: AtomicU64 = AtomicU64::new(0);

#[derive(Trace)]
enum GcTree {
    Leaf(GcCell<Option<Gc<GcTree>>>),
    Knot(Gc<GcTree>, Gc<GcTree>, u8),
}

impl GcTree {
    pub fn new(depth: u32) -> Gc<GcTree> {
        A.fetch_add(1, Ordering::Relaxed);
        if depth == 0 {
            return Gc::new(GcTree::Leaf(GcCell::new(None)));
        }
        return Gc::new(GcTree::Knot(
            GcTree::new(depth - 1),
            GcTree::new(depth - 1),
            10,
        ));
    }

    fn inject(&self, r: Gc<GcTree>) {
        match self {
            GcTree::Leaf(leaf) => {
                leaf.borrow_mut().replace(r);
            }
            GcTree::Knot(k, _, _) => {
                k.inject(r);
            }
        }
    }
}

impl Finalize for GcTree {
    fn finalize(&self) {
        A.fetch_sub(1, Ordering::Relaxed);
    }
}

fn main() {
    println!("Creating tree...");
    let mut before = std::time::Instant::now();
    let tree = GcTree::new(21);
    let tree2 = tree.clone();
    tree2.inject(tree2.clone());
    let mut after = std::time::Instant::now();
    println!(
        "Done after {}ms. Running gc...",
        (after - before).as_millis()
    );

    before = std::time::Instant::now();
    gc::force_collect();

    after = std::time::Instant::now();

    println!(
        "Done with gc scan after {}ms. Releasing Objects...",
        (after - before).as_millis()
    );

    println!("Created objects: {}", A.load(Ordering::Relaxed));
    std::mem::drop(tree);
    before = std::time::Instant::now();
    gc::force_collect();
    std::mem::drop(tree2);
    gc::force_collect();

    after = std::time::Instant::now();

    println!(
        "Done with gc cleanup after {}ms.",
        (after - before).as_millis()
    );

    println!("Done. Remaining objects: {}", A.load(Ordering::Relaxed));
}
