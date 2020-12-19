#![feature(drain_filter)]

use crate::gc::{Gc, GcCell, Mark, Mutability};
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::atomic::{AtomicU64, Ordering};

mod gc;

static A: AtomicU64 = AtomicU64::new(0);

enum GcTree {
    Leaf(GcCell<Option<Gc<GcTree>>>),
    Knot(Gc<GcTree>, Gc<GcTree>, u8),
}

impl Mark for GcTree {
    fn mark_all(&self, generation: u64) {
        match self {
            GcTree::Leaf(leaf) => {
                leaf.mark_all(generation);
            }
            GcTree::Knot(a, b, c) => {
                a.mark_all(generation);
                b.mark_all(generation);
                c.mark_all(generation);
            }
        }
    }

    fn unroot(&self) -> Mutability {
        match self {
            GcTree::Leaf(leaf) => leaf.unroot(),
            GcTree::Knot(a, b, c) => a.unroot().or(&b.unroot().or(&c.unroot())),
        }
    }

    fn root(&self) {
        match self {
            GcTree::Leaf(leaf) => {
                leaf.root();
            }
            GcTree::Knot(a, b, c) => {
                a.root();
                b.root();
                c.root();
            }
        }
    }

    fn destroy(&self) {
        match self {
            GcTree::Leaf(leaf) => leaf.destroy(),
            GcTree::Knot(a, b, c) => {
                a.destroy();
                b.destroy();
                c.destroy();
            }
        }
    }
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
                Gc::borrow(k).inject(r);
            }
        }
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
    let tree = GcTree::new(21);
    let tree2 = tree.clone();
    tree2.inject(tree2.clone());
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
    Gc::collect();
    after = std::time::Instant::now();

    println!(
        "Done with gc cleanup after {}ms.",
        (after - before).as_millis()
    );

    println!("Done. Remaining objects: {}", A.load(Ordering::Relaxed));
}
