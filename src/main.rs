use crate::gc::{Gc, Mark};
use std::cell::RefCell;
use std::time::Duration;

mod gc;
mod identity;

struct A {
    a: Gc<String>,
    b: RefCell<Gc<B>>
}

struct B {
    b: Gc<String>
}

impl Mark for A {
    fn mark_all(&self) {
        self.a.mark_all();
        self.b.mark_all();
    }
}

impl Mark for B {
    fn mark_all(&self) {
        self.b.mark_all()
    }
}

impl Drop for B {
    fn drop(&mut self) {
        println!("Dropping")
    }
}

fn main() {
    let a = Gc::new(A { a: Gc::new("A".into()), b: RefCell::from(Gc::new(B { b: Gc::new("B".into()) })) });
    println!("{}", (&a.borrow().b.borrow()).borrow().b);
    Gc::collect();
    println!("{}", (&a.borrow().b.borrow()).borrow().b);
    a.borrow().b.replace(Gc::new(B { b: Gc::new("C".into()) }));
    println!("{}", (&a.borrow().b.borrow()).borrow().b);
    Gc::collect();
    Gc::collect();
    std::thread::sleep(Duration::new(4, 0));
}
