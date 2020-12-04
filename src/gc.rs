use std::collections::{HashSet, HashMap, VecDeque, BTreeMap, BTreeSet, BinaryHeap};
use crate::identity::Identity;
use std::rc::{Rc, Weak};
use std::any::Any;
use std::cell::{Cell, RefCell};
use std::path::{Path, PathBuf};
use std::num::*;
use std::sync::atomic::*;
use std::ops::Deref;
use std::marker::PhantomData;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter, Pointer, Debug};
use std::hash::{Hash, Hasher};

thread_local! {
    static ENGINE: GcEngine = GcEngine::new();
}

pub struct GcEngine {
    generation: Cell<u64>,
    group:  Cell<u64>,
    is_finding_roots: Cell<bool>,
    refs: RefCell<HashMap<Identity, Rc<dyn PossiblyRooted>>>,
}

impl GcEngine {
    fn new() -> GcEngine {
        GcEngine { generation: Cell::new(0), group: Cell::new(0), is_finding_roots: Cell::new(false), refs: RefCell::new(Default::default()) }
    }

    pub fn collect(&self) {
        self.generation.set(self.generation.get() + 1);
        for (_, entity) in self.refs.borrow().iter() {
            entity.mark_all();
            self.group.set(self.group.get() + 2);
        }
        self.generation.set(self.group.get());
        self.group.set(self.group.get() + 2);
        self.is_finding_roots.set(true);
        for (_, entity) in self.refs.borrow().iter() {
            if entity.is_rooted(self.group.get() % 2 == 0) {
                entity.mark_all();
            }
        }
        self.is_finding_roots.set(false);
        let mut to_remove = Vec::new();
        for (id, entity) in self.refs.borrow().iter() {
            if entity.get_last_marking_gen() != self.group.get() {
                to_remove.push(id.clone());
            }
        }
        let mut mut_borrow = self.refs.borrow_mut();
        for remove_id in &to_remove {
            (&mut mut_borrow).remove(remove_id);
        }
        self.generation.set(self.generation.get() + 2);
        self.group.set(self.generation.get());
    }
}

pub struct GcInner<T: Mark> {
    data: T,
    last_marking_gen: Cell<u64>,
    times_marked_even: Cell<u64>,
    times_marked_uneven: Cell<u64>,
    self_pointer: Cell<Option<Weak<dyn Any>>>,
}

trait PossiblyRooted: Mark {
    fn is_rooted(&self, generation_is_even: bool) -> bool;
    fn get_last_marking_gen(&self) -> u64;
}
impl <T: Mark> PossiblyRooted for GcInner<T> {
    // Has been marked by:
    // + 1 the gc
    // + n neighbours pointing at us
    // Has weak refcount of self_ptr:
    // + 1 for self counter
    // + n neighbours pointing at us
    // + m roots pointed at us
    // -> if self_ptr has more refs than we have marks, we are rooted.
    fn is_rooted(&self, generation_is_even: bool) -> bool {
        let times_marked = if generation_is_even { self.times_marked_even.get() } else {self.times_marked_uneven.get() };
        let self_ptr_temp = self.self_pointer.take();
        let ret = self_ptr_temp.as_ref().unwrap().weak_count() > times_marked as usize;
        self.self_pointer.set(self_ptr_temp);
        return ret;
    }

    fn get_last_marking_gen(&self) -> u64 {
        self.last_marking_gen.get()
    }
}

pub struct Gc<T: Mark> {
    inner: Weak<GcInner<T>>,
}

pub struct GcRef<'a, T: Mark> {
    ptr: Rc<GcInner<T>>,
    marker: PhantomData<&'a ()> // Don't you dare breaking my gc ;)
}

impl <'a, T: Mark> Deref  for GcRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.ptr.data.borrow()
    }
}

impl<T: Mark> Gc<T> {
    pub fn borrow(&self) -> GcRef<T> {
        GcRef {
            ptr: self.inner.upgrade().unwrap(),
            marker: Default::default()
        }
    }
}

impl Gc<()> {
    pub fn collect() {
        ENGINE.with(|e| e.collect())
    }
}


impl <T: Mark + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Gc::borrow(self).fmt(f)
    }
}

impl <T: Mark + Hash> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::borrow(self).hash(state)
    }
}

impl <T: Mark + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        Gc::borrow(self).deref() == Gc::borrow(other).deref()
    }
}

impl <T: Mark + Eq + PartialEq> Eq for Gc<T> {

}

impl <T: Mark + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (&Gc::borrow(self).ptr).fmt(f)
    }
}

impl <'a, T: Mark> AsRef<T> for GcRef<'a, T> {
    fn as_ref(&self) -> &T {
        self.ptr.data.borrow()
    }
}

impl <T: Mark + 'static> Gc<T> {
    pub fn new(value: T) -> Gc<T> {
        let inner = GcInner {
            data: value,
            last_marking_gen: Cell::new(0),
            times_marked_even: Cell::new(0),
            times_marked_uneven: Cell::new(0),
            self_pointer: Cell::new(None)
        };
        let strong = Rc::new(inner);
        strong.self_pointer.set(Some(Rc::downgrade(&(strong.clone() as Rc<dyn Any>))));
        ENGINE.with(|engine| {
            engine.refs.borrow_mut().insert(Identity::new(), strong.clone() as Rc<dyn PossiblyRooted>);
        });
        return Gc { inner: Rc::downgrade(&strong) }
    }
}

pub trait Mark {
    fn mark_all(&self);
}

impl<T: Mark> Mark for Gc<T> {
    fn mark_all(&self) {
        self.inner.upgrade().unwrap().mark_all()
    }
}

impl<T: Mark> Mark for GcInner<T> {
    fn mark_all(&self) {
        let (generation, group, is_finding_roots) = ENGINE.with(|c| (c.generation.get(), c.group.get(), c.is_finding_roots.get()));
        if is_finding_roots {
            if generation > self.last_marking_gen.get() {
                if generation % 2 == 0 {
                    self.times_marked_even.set(self.times_marked_even.get() + 1);
                    self.times_marked_uneven.set(0);
                } else {
                    self.times_marked_uneven.set(self.times_marked_uneven.get() + 1);
                    self.times_marked_even.set(0);
                }
            }
        }
        let should_spread = self.last_marking_gen.replace(group) < generation;
        if should_spread {
            self.data.mark_all();
        }
    }
}

impl <T: Mark> Mark for RefCell<T> {
    fn mark_all(&self) {
        self.borrow().mark_all();
    }
}

impl <T: Mark> Mark for Box<T> {
    fn mark_all(&self) {
        Box::deref(self).mark_all()
    }
}

impl <T: Mark> Mark for Rc<T> {
    fn mark_all(&self) {
        Rc::deref(self).mark_all()
    }
}

impl <T: Mark, E: Mark> Mark for Result<T, E> {
    fn mark_all(&self) {
        match self {
            Ok(ok) => {ok.mark_all()}
            Err(err) => {err.mark_all()}
        }
    }
}

impl <T: Mark> Mark for Option<T> {
    fn mark_all(&self) {
        if let Some(this) = self {
            this.mark_all()
        }
    }
}

macro_rules! iter_impl {
    ($ty:ty) => {
        impl <T: Mark> Mark for $ty {
            fn mark_all(&self) {
                for item in self.iter() {
                    item.mark_all()
                }
            }
        }
    };
}

// Note: Not doing impl for Iterator yet until specialization comes around
iter_impl!(Vec<T>);
iter_impl!(HashSet<T>);
iter_impl!(VecDeque<T>);
iter_impl!(BTreeSet<T>);
iter_impl!(BinaryHeap<T>);

impl <T: Mark, V: Mark> Mark for HashMap<T, V> {
    fn mark_all(&self) {
        for item in self.iter() {
            item.0.mark_all();
            item.1.mark_all()
        }
    }
}

impl <K: Mark, V: Mark> Mark for BTreeMap<K, V> {
    fn mark_all(&self) {
        for item in self.iter() {
            item.0.mark_all();
            item.1.mark_all();
        }
    }
}
macro_rules! empty_mark {
    ($($T:ty),*) => {
        $(
            impl Mark for $T {
                fn mark_all(&self) {}
            }
        )*
    }
}



macro_rules! array_mark_impls {
    ($($n:expr),*) => {
        $(
            iter_impl!([T; $n]);
        )*
    }
}

array_mark_impls![
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31
];

macro_rules! tuple_marks {
    ($(($($args:ident),*);)*) => {
        $(
            impl <$($args: $crate::gc::Mark),*> Mark for ($($args,)*) {
                fn mark_all(&self) {
                    mod inner_hack {
                        #[allow(non_snake_case)]
                        pub(crate) fn mark_all<$($args: $crate::gc::Mark),*>(&($(ref $args,)*): &($($args,)*)) {
                            $(($args).mark_all();)*
                        }
                    }
                    inner_hack::mark_all(&self);
                }
            }
        )*
    }
}

tuple_marks![
    (A);
    (A, B);
    (A, B, C);
    (A, B, C, D);
    (A, B, C, D, E);
    (A, B, C, D, E, F);
    (A, B, C, D, E, F, G);
    (A, B, C, D, E, F, G, H);
    (A, B, C, D, E, F, G, H, I);
    (A, B, C, D, E, F, G, H, I, J);
    (A, B, C, D, E, F, G, H, I, J, K);
    (A, B, C, D, E, F, G, H, I, J, K, L);
];


macro_rules! simple_no_mark {
    ($($ty:ty),*) => {
        $(
            empty_mark!($ty);
        )*
    }
}
simple_no_mark![
    (),
    str,
    bool,
    isize,
    usize,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    i128,
    u128,
    f32,
    f64,
    char,
    String,
    Path,
    PathBuf,
    NonZeroIsize,
    NonZeroUsize,
    NonZeroI8,
    NonZeroU8,
    NonZeroI16,
    NonZeroU16,
    NonZeroI32,
    NonZeroU32,
    NonZeroI64,
    NonZeroU64,
    NonZeroI128,
    NonZeroU128,
    AtomicBool,
    AtomicIsize,
    AtomicUsize,
    AtomicI8,
    AtomicU8,
    AtomicI16,
    AtomicU16,
    AtomicI32,
    AtomicU32,
    AtomicI64,
    AtomicU64
];
