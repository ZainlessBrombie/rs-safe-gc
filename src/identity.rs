use std::rc::Rc;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::cell::Cell;
use std::borrow::Borrow;


#[derive(Clone)]
pub struct Identity(Rc<Cell<u64>>);

impl Identity {
    pub fn new() -> Identity {
        let ret = Identity(Rc::new(Cell::new(0)));
        ret.0.set(Rc::borrow(&ret.0) as *const Cell<_> as u64);
        return ret;
    }
}

impl Hash for Identity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(Rc::deref(&self.0).get())
    }
}

impl PartialEq for Identity {
    fn eq(&self, other: &Self) -> bool {
        let us_loc: u64 = Rc::deref(&self.0).get();
        let other_loc: u64 = Rc::as_ref(&other.0).get();

        return (us_loc as *const u64) == (other_loc as *const u64);
    }
}

impl Eq for Identity {}
