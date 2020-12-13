use std::collections::{HashSet, HashMap, VecDeque, BTreeMap, BTreeSet, BinaryHeap, LinkedList};
use crate::identity::Identity;
use std::rc::{Rc, Weak};
use std::any::Any;
use std::cell::{Cell, RefCell, Ref, RefMut, BorrowMutError};
use std::path::{Path, PathBuf};
use std::num::*;
use std::sync::atomic::*;
use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter, Pointer, Debug};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};

thread_local! {
    static ENGINE: GcEngine = GcEngine::new();
}

pub struct GcEngine {
    generation: Cell<usize>,
    group: Cell<usize>,
    is_finding_roots: Cell<bool>,
    refs: RefCell<LinkedList<Rc<dyn PossiblyRooted>>>,
}

impl<T: PossiblyRooted> PossiblyRooted for RefCell<Option<T>> {
    fn is_rooted(&self) -> bool {
        if let Ok(us) = RefCell::try_borrow(self) {
            if let Some(us) = us.deref().as_ref() {
                return us.is_rooted();
            } else {
                return false;
            }
        } else {
            return true;
        }
    }

    fn get_last_marking_gen(&self) -> usize {
        if let Some(us) = RefCell::borrow(self).deref().as_ref() { // TODO borrow panic
            return us.get_last_marking_gen();
        } else {
            return 0;
        }
    }

    fn reset_markers(&self) {
        if let Some(us) = RefCell::borrow(self).deref().as_ref() { // TODO
            return us.reset_markers();
        }
    }

    fn is_dead(&self) -> bool {
        if let Some(us) = RefCell::borrow(self).deref().as_ref() {
            return us.is_dead();
        } else {
            true
        }
    }

    fn mark_all(&self) {
        unimplemented!()
    }
}

impl GcEngine {
    fn new() -> GcEngine {
        GcEngine { generation: Cell::new(0), group: Cell::new(0), is_finding_roots: Cell::new(false), refs: RefCell::new(Default::default()) }
    }

    pub fn collect(&self) {
        // Reset markers
        for _ in self.refs.borrow_mut().drain_filter(|entity| {
            if entity.is_dead() {
                true;
            }
            entity.reset_markers();
            false
        }) {}
        self.generation.set(self.generation.get() + 1);
        // Count connections
        for entity in self.refs.borrow().iter() {
            entity.deref().mark_all();
            self.group.set(self.group.get() + 2);
        }

        self.generation.set(self.group.get() + 2);
        self.group.set(self.group.get() + 2);

        // Mark roots
        self.is_finding_roots.set(true);
        for entity in self.refs.borrow().iter() {
            if entity.is_rooted() {
                entity.mark_all();
            }
        }
        self.is_finding_roots.set(false);

        let self_group = self.group.get();
        for _ in self.refs.borrow_mut().drain_filter(|entity| {
            if entity.get_last_marking_gen() != self_group || entity.is_dead() {
                return true;
            }
            false
        }) {}
        self.generation.set(self.generation.get() + 2);
        self.group.set(self.generation.get());
    }
}

pub struct Gc<T: Mark> {
    inner: Weak<GcInner<T>>,
    dropper: Dropper<T>,
}

pub struct GcInner<T: Mark> {
    data: T,
    last_marking_gen: AtomicUsize,
    // times_marked: AtomicUsize,
    self_pointer: Cell<Option<Weak<dyn Any>>>,
    id: Identity,
    root_count: AtomicUsize,
}

trait PossiblyRooted {
    fn is_rooted(&self) -> bool;
    fn get_last_marking_gen(&self) -> usize;
    fn reset_markers(&self);
    fn is_dead(&self) -> bool;
    fn mark_all(&self);
}

impl<T: Mark> PossiblyRooted for GcInner<T> {
    // Has been marked by:
    // + 1 the gc
    // + n neighbours pointing at us
    // Has weak refcount of self_ptr:
    // + 1 for self counter
    // + 1 for dropper
    // + n neighbours pointing at us
    // + m roots pointed at us
    // -> if self_ptr has more refs than we have marks, we are rooted.
    fn is_rooted(&self) -> bool {
        self.root_count.load(Ordering::Relaxed) > 0
    }

    fn get_last_marking_gen(&self) -> usize {
        self.last_marking_gen.load(Ordering::Relaxed)
    }

    fn reset_markers(&self) {
        //self..store(0, Ordering::Relaxed);
    }

    fn is_dead(&self) -> bool {
        //self.data.borrow().is_none()
        unimplemented!()
    }

    fn mark_all(&self) {
        unimplemented!()
    }
}

pub struct GcCell<T: Mark> {
    internal_cell: RefCell<T>
}

impl <T:Mark> GcCell<T> {
    pub fn new(o: T) -> GcCell<T> {
        GcCell { internal_cell: RefCell::new(o) }
    }

    pub fn borrow(&self) -> GcCellRef<T> {
        GcCellRef { r: self.internal_cell.borrow() }
    }

    pub fn try_borrow_mut(&self) -> Result<GcCellRefMut<T>, BorrowMutError> {
        self.internal_cell.try_borrow_mut()
            .map(|b| {
                GcCellRefMut { rm: b }
            })
    }

    pub fn borrow_mut(&self) -> GcCellRefMut<T> {
        GcCellRefMut { rm: self.internal_cell.borrow_mut() }
    }
}

impl <T: Mark> Mark for GcCell<T> {
    fn mark_all(&self) {
        unimplemented!()
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}

pub struct GcCellRef<'a, T> where T: Mark {
    r: Ref<'a, T>,
}

pub struct GcCellRefMut<'a, T: Mark> {
    rm: RefMut< 'a,T>
}

impl <'a, T>Deref for GcCellRef<'a, T> where T: Mark {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.r.deref()
    }
}

impl <'a, T>Deref for GcCellRefMut<'a, T> where T: Mark {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.rm.deref()
    }
}

impl <'a, T>DerefMut for GcCellRefMut<'a, T> where T: Mark {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.rm.deref_mut()
    }
}

impl <'a, T> GcCellRef<'a, T> where T: Mark {

}

impl <'a, T> GcCellRefMut<'a, T> where T: Mark {

}

impl<T: Mark> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc {
            inner: self.inner.clone(),
            dropper: self.dropper.clone(),
        }
    }
}

pub struct GcRef<'a, T: Mark> {
    ptr: Rc<GcInner<T>>,
    marker: PhantomData<&'a ()>, // Don't you dare breaking my gc ;)
}

impl<'a, T: Mark> Deref for GcRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: Mark> Gc<T> {
    pub fn borrow(&self) -> GcRef<T> {
        let rc = self.inner.upgrade().unwrap();
        GcRef {
            ptr: rc,
            marker: Default::default(),
        }
    }
}

impl Gc<()> {
    pub fn collect() {
        ENGINE.with(|e| e.collect())
    }
}


impl<T: Mark + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Gc::borrow(self).fmt(f)
    }
}

impl<T: Mark + Hash> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::borrow(self).hash(state)
    }
}

impl<T: Mark + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        Gc::borrow(self).deref() == Gc::borrow(other).deref()
    }
}

impl<T: Mark + Eq + PartialEq> Eq for Gc<T> {}

impl<T: Mark + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (&Gc::borrow(self).ptr).fmt(f)
    }
}

impl<'a, T: Mark> AsRef<T> for GcRef<'a, T> {
    fn as_ref(&self) -> &T {
        &self.ptr.data
    }
}

impl <T:Debug + Mark> Debug for GcCell<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        GcCell::borrow(self).fmt(f)
    }
}

impl<T: Mark + 'static> Gc<T> {
    pub fn new(value: T) -> Gc<T> {
        let id = Identity::new();
        let inner = GcInner {
            data: value,
            last_marking_gen: Default::default(),
            self_pointer: Cell::new(None),
            id: id.clone(),
            root_count: AtomicUsize::new(1),
        };
        let strong = Rc::new(inner);
        strong.self_pointer.set(Some(Rc::downgrade(&(strong.clone() as Rc<dyn Any>))));
        ENGINE.with(|engine| {
            engine.refs.borrow_mut().push_back(strong.clone() as Rc<dyn PossiblyRooted>);
        });
        return Gc { inner: Rc::downgrade(&strong), dropper: Dropper { parent: Rc::downgrade(&strong) } };
    }
}

struct Dropper<T: Mark> {
    parent: Weak<GcInner<T>>
}

impl<T: Mark> Clone for Dropper<T> {
    fn clone(&self) -> Self {
        Dropper { parent: self.parent.clone() }
    }
}

impl<T: Mark> Drop for Dropper<T> {
    fn drop(&mut self) {
        if self.parent.weak_count() == 2 { // Us and itself
            //ENGINE.with(|e| {
                // let keep_alive = self.parent.upgrade().unwrap().data.take();
                //std::mem::drop(keep_alive);
                // TODO unimplemented!()
            //})
        }
    }
}

pub trait Mark {
    fn mark_all(&self);

    fn unroot(&self);

    fn root(&self); // Note: Rc's are always considered roots
}

impl<T: Mark> Mark for Gc<T> {
    fn mark_all(&self) {
        self.inner.upgrade().unwrap().mark_all()
    }

    fn unroot(&self) {
        self.inner.upgrade().unwrap().unroot()
    }

    fn root(&self) {
        self.inner.upgrade().unwrap().root()
    }
}

impl<T: Mark> Mark for GcInner<T> {
    fn mark_all(&self) {
        let (generation, group, is_finding_roots) = ENGINE.with(|c| (c.generation.get(), c.group.get(), c.is_finding_roots.get()));
        if is_finding_roots && generation > self.last_marking_gen.load(Ordering::Relaxed) {
            // todo self.times_marked.fetch_add(1, Ordering::Relaxed);
        }
        let should_spread = self.last_marking_gen.swap(group, Ordering::Relaxed) < generation;
        if should_spread {
            self.data.mark_all();
        }
    }

    fn unroot(&self) {
        self.root_count.fetch_sub(1, Ordering::Relaxed);
    }

    fn root(&self) {
        self.root_count.fetch_add(1, Ordering::Relaxed);
    }
}


impl<T: Mark + ?Sized> Mark for Box<T> {
    fn mark_all(&self) {
        Box::deref(self).mark_all()
    }

    fn unroot(&self) {
        Box::deref(self).unroot()
    }

    fn root(&self) {
        Box::deref(self).unroot()
    }
}

impl<T: Mark + ?Sized> Mark for Rc<T> {
    fn mark_all(&self) {
        Rc::deref(self).mark_all()
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}

impl <T: Mark + ?Sized> Mark for Arc<T> {
    fn mark_all(&self) {
        unimplemented!()
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}

impl <T: Mark> Mark for Mutex<T> {
    fn mark_all(&self) {
        unimplemented!()
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}

impl<T: Mark, E: Mark> Mark for Result<T, E> {
    fn mark_all(&self) {
        match self {
            Ok(ok) => { ok.mark_all() }
            Err(err) => { err.mark_all() }
        }
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}

impl<T: Mark> Mark for Option<T> {
    fn mark_all(&self) {
        if let Some(this) = self {
            this.mark_all()
        }
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
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

            fn unroot(&self) {
                unimplemented!()
            }

            fn root(&self) {
                unimplemented!()
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

impl<T: Mark, V: Mark> Mark for HashMap<T, V> {
    fn mark_all(&self) {
        for item in self.iter() {
            item.0.mark_all();
            item.1.mark_all()
        }
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}

impl<K: Mark, V: Mark> Mark for BTreeMap<K, V> {
    fn mark_all(&self) {
        for item in self.iter() {
            item.0.mark_all();
            item.1.mark_all();
        }
    }

    fn unroot(&self) {
        unimplemented!()
    }

    fn root(&self) {
        unimplemented!()
    }
}
macro_rules! empty_mark {
    ($($T:ty),*) => {
        $(
            impl Mark for $T {
                fn mark_all(&self) {}

                fn unroot(&self) {
                    unimplemented!()
                }

                fn root(&self) {
                    unimplemented!()
                }
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

                fn unroot(&self) {
                    unimplemented!()
                }

                fn root(&self) {
                    unimplemented!()
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
