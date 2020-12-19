use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::*;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use std::sync::atomic::*;

thread_local! {
    static ENGINE: GcEngine = GcEngine::new();
}

pub struct GcEngine {
    generation: Cell<u64>,
    direct_cells: RefCell<LinkedList<Weak<dyn ConditionallyDestroyable>>>,
    root_counted: RefCell<LinkedList<Weak<dyn Markable>>>,
}

impl GcEngine {
    fn new() -> GcEngine {
        GcEngine {
            generation: Cell::new(0),
            direct_cells: RefCell::new(Default::default()),
            root_counted: RefCell::new(Default::default()),
        }
    }

    pub fn collect(&self) {
        let generation = self.generation.get() + 1;
        self.generation.set(generation);

        self.root_counted
            .borrow_mut()
            .drain_filter(|el| {
                if let Some(el) = el.upgrade() {
                    el.mark_if_rooted(generation);
                    return false;
                } else {
                    return true;
                }
            })
            .filter(|_| false)
            .next();

        self.direct_cells
            .borrow_mut()
            .drain_filter(|el| {
                if let Some(el) = el.upgrade() {
                    el.destroy_conditionally(generation)
                } else {
                    return true;
                }
            })
            .filter(|_| false)
            .next();
    }
}

#[derive(Copy, Clone)]
pub enum Mutability {
    None,
    Deep,
    Shallow,
}

impl Mutability {
    pub fn is_root_counted(&self) -> bool {
        match self {
            Mutability::None => false,
            Mutability::Deep => true,
            Mutability::Shallow => true,
        }
    }

    pub fn or(&self, other: &Mutability) -> Mutability {
        match (self, other) {
            (Mutability::Shallow, _) => Mutability::Shallow,
            (_, Mutability::Shallow) => Mutability::Shallow,
            (Mutability::Deep, _) => Mutability::Deep,
            (_, Mutability::Deep) => Mutability::Deep,
            (_, _) => Mutability::None,
        }
    }
}

pub struct Gc<T: Mark> {
    inner: Rc<GcInner<T>>,
    is_rooted: Cell<bool>,
}

struct GcInner<T: Mark> {
    value: T,
    mutability: Mutability,
    root_count: Cell<u64>,
    // Not strictly necessary, could be kept in GcCells
    // - but then two Gc referencing a complex net would traverse it twice
    last_generation: Cell<u64>,
}

impl<T: Mark + 'static> Gc<T> {
    pub fn new(value: T) -> Gc<T> {
        let mutability = value.unroot();
        let inner = Rc::new(GcInner {
            value,
            mutability,
            root_count: Cell::new(1),
            last_generation: Cell::new(0),
        });
        match mutability {
            Mutability::None => {}
            Mutability::Deep => {
                let weak = Rc::downgrade(&inner);
                ENGINE.with(|e| {
                    e.root_counted.borrow_mut().push_back(weak);
                })
            }
            Mutability::Shallow => {
                let weak = Rc::downgrade(&inner);
                ENGINE.with(|e| {
                    e.root_counted.borrow_mut().push_back(weak.clone());
                    e.direct_cells.borrow_mut().push_back(weak);
                })
            }
        }
        Gc {
            inner,
            is_rooted: Cell::new(true),
        }
    }
}

impl<T: Mark> Clone for Gc<T> {
    fn clone(&self) -> Self {
        self.inner.root();
        Gc {
            inner: self.inner.clone(),
            is_rooted: Cell::new(true),
        }
    }
}

impl<T: Mark> Drop for Gc<T> {
    fn drop(&mut self) {
        self.unroot();
    }
}

impl<T: Mark> Mark for Gc<T> {
    fn mark_all(&self, generation: u64) {
        self.inner.mark_all(generation);
    }

    fn unroot(&self) -> Mutability {
        if self.is_rooted.get() {
            self.is_rooted.set(false);
            self.inner.unroot();
        }
        if self.inner.mutability.is_root_counted() {
            Mutability::Deep
        } else {
            Mutability::None
        }
    }

    fn root(&self) {
        if !self.is_rooted.get() {
            self.is_rooted.set(true);
            self.inner.root();
        }
    }

    fn destroy(&self) {
        // Do nothing. The reference that kept us alive is being destroyed, so we will be dropped automatically
    }
}

pub struct GcCell<T: Mark> {
    inner: RefCell<NaiveOptional<T>>,
}

impl<T: Mark> GcCell<T> {
    pub fn new(o: T) -> GcCell<T> {
        o.unroot();
        GcCell {
            inner: RefCell::new(NaiveOptional::Some(o)),
        }
    }

    pub fn borrow(&self) -> GcRef<T> {
        GcRef {
            inner: self.inner.borrow(),
        }
    }

    pub fn borrow_mut(&self) -> GcRefMut<T> {
        let ref_mut = self.inner.borrow_mut();
        ref_mut.root();
        GcRefMut { inner: ref_mut }
    }

    pub fn into_inner(self) -> T {
        match self.inner.into_inner() {
            NaiveOptional::Some(o) => o,
            NaiveOptional::None => {
                panic!("Cannot convert already collected GcCell into inner! (Incorrect implementation of Mark somewhere)")
            }
        }
    }
}

pub struct GcRef<'a, T: Mark> {
    inner: Ref<'a, NaiveOptional<T>>,
}

impl<'a, T: Mark> Drop for GcRefMut<'a, T> {
    fn drop(&mut self) {
        self.inner.borrow().unroot();
    }
}

impl<'a, T: Mark> Deref for GcRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<'a, T: Mark> Deref for GcRefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<'a, T: Mark> DerefMut for GcRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.deref_mut()
    }
}

enum NaiveOptional<T> {
    Some(T),
    None,
}

impl<T> Default for NaiveOptional<T> {
    fn default() -> Self {
        NaiveOptional::None
    }
}

impl<T> Deref for NaiveOptional<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            NaiveOptional::Some(some) => some,
            NaiveOptional::None => {
                panic!("This value has been cleaned up already!")
            }
        }
    }
}

impl<T> DerefMut for NaiveOptional<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            NaiveOptional::Some(some) => some,
            NaiveOptional::None => {
                panic!("This value has been cleaned up already!")
            }
        }
    }
}

pub struct GcRefMut<'a, T: Mark> {
    inner: RefMut<'a, NaiveOptional<T>>,
}

impl<T: Mark> Mark for GcCell<T> {
    fn mark_all(&self, generation: u64) {
        // If we are borrowed, we have a lifetime, which in turn relates to the stack somehow
        if let Ok(inner) = self.inner.try_borrow() {
            inner.mark_all(generation);
        }
    }

    fn unroot(&self) -> Mutability {
        // We don't care about being rooted ourselves. Our content does, and we manage that ourselves
        Mutability::Shallow
    }

    fn root(&self) {
        // Same as with unroot
    }

    fn destroy(&self) {
        std::mem::take((self.inner.try_borrow_mut()
            .expect(
                "Supposedly unused GcCell was still being used. Mark must have been implemented incorrectly somewhere."
            )
        ).deref_mut());
    }
}

impl Gc<()> {
    pub fn collect() {
        ENGINE.with(|e| e.collect())
    }
}

impl<T: Mark> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner.deref().value
    }
}

impl<T: Mark + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.value.fmt(f)
    }
}

impl<T: Mark + Hash> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.value.hash(state)
    }
}

impl<T: Mark + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        &self.inner.value == &other.inner.value
    }
}

impl<T: Mark + Eq + PartialEq> Eq for Gc<T> {}

impl<T: Mark + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.value.fmt(f)
    }
}

impl<'a, T: Mark> AsRef<T> for GcRef<'a, T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T: Debug + Mark> Debug for GcCell<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.borrow().deref().fmt(f)
    }
}

pub trait Mark {
    fn mark_all(&self, generation: u64);

    fn unroot(&self) -> Mutability;

    fn root(&self);

    fn destroy(&self);
}

impl<T: Mark> Mark for &T {
    fn mark_all(&self, generation: u64) {
        (*self).mark_all(generation)
    }

    fn unroot(&self) -> Mutability {
        (*self).unroot()
    }

    fn root(&self) {
        (*self).root()
    }

    fn destroy(&self) {
        (*self).destroy()
    }
}

impl<T: Mark> Mark for GcInner<T> {
    fn mark_all(&self, generation: u64) {
        if !self.mutability.is_root_counted() {
            return;
        }
        let last_gen = self.last_generation.get();
        if last_gen == generation {
            return;
        }
        self.last_generation.set(generation);
        self.value.mark_all(generation);
    }

    fn unroot(&self) -> Mutability {
        if !self.mutability.is_root_counted() {
            return self.mutability;
        }
        let r_count = self.root_count.get();
        self.root_count.set(r_count - 1);
        self.mutability
    }

    fn root(&self) {
        if !self.mutability.is_root_counted() {
            return;
        }
        let r_count = self.root_count.get();
        self.root_count.set(r_count + 1);
    }

    fn destroy(&self) {
        unreachable!()
    }
}

trait ConditionallyDestroyable {
    fn destroy_conditionally(&self, generation: u64) -> bool;
}

trait Markable {
    fn mark_if_rooted(&self, generation: u64);
}

impl<T: Mark> Markable for GcInner<T> {
    fn mark_if_rooted(&self, generation: u64) {
        if self.root_count.get() != 0 {
            self.mark_all(generation);
        }
    }
}

impl<T: Mark> ConditionallyDestroyable for GcInner<T> {
    fn destroy_conditionally(&self, generation: u64) -> bool {
        if self.last_generation.get() == generation {
            return false;
        } else {
            self.value.destroy();
        }
        return true;
    }
}

impl<T: Mark + ?Sized> Mark for Box<T> {
    fn mark_all(&self, generation: u64) {
        Box::deref(self).mark_all(generation)
    }

    fn unroot(&self) -> Mutability {
        Box::deref(self).unroot()
    }

    fn root(&self) {
        Box::deref(self).root();
    }

    fn destroy(&self) {
        Box::deref(self).destroy();
    }
}

impl<T: Mark + ?Sized> Mark for Rc<T> {
    fn mark_all(&self, generation: u64) {
        Rc::deref(self).mark_all(generation)
    }

    fn unroot(&self) -> Mutability {
        Rc::deref(self).unroot()
    }

    fn root(&self) {
        Rc::deref(self).root()
    }

    fn destroy(&self) {
        Rc::deref(self).destroy();
    }
}

impl<T: Mark, E: Mark> Mark for Result<T, E> {
    fn mark_all(&self, generation: u64) {
        match self {
            Ok(ok) => ok.mark_all(generation),
            Err(err) => err.mark_all(generation),
        }
    }

    fn unroot(&self) -> Mutability {
        match self {
            Ok(ok) => ok.unroot(),
            Err(err) => err.unroot(),
        }
    }

    fn root(&self) {
        match self {
            Ok(ok) => ok.root(),
            Err(err) => err.root(),
        }
    }

    fn destroy(&self) {
        match self {
            Ok(ok) => ok.destroy(),
            Err(err) => err.destroy(),
        }
    }
}

impl<T: Mark> Mark for Option<T> {
    fn mark_all(&self, generation: u64) {
        if let Some(this) = self {
            this.mark_all(generation)
        }
    }

    fn unroot(&self) -> Mutability {
        if let Some(this) = self {
            this.unroot()
        } else {
            // We may contain a None<GcCell> - but it cannot become a Some if we are a Gc
            Mutability::None
        }
    }

    fn root(&self) {
        if let Some(this) = self {
            this.root()
        }
    }

    fn destroy(&self) {
        if let Some(this) = self {
            this.destroy()
        }
    }
}

macro_rules! iter_impl {
    ($ty:ty) => {
        impl<T: Mark> Mark for $ty {
            fn mark_all(&self, generation: u64) {
                for item in self.iter() {
                    item.mark_all(generation)
                }
            }

            fn unroot(&self) -> Mutability {
                let mut mutability = Mutability::None;
                for item in self.iter() {
                    mutability = item.unroot().or(&mutability);
                }
                return mutability;
            }

            fn root(&self) {
                for item in self.iter() {
                    item.root();
                }
            }

            fn destroy(&self) {
                for item in self.iter() {
                    item.destroy();
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

impl<T: Mark, V: Mark> Mark for HashMap<T, V> {
    fn mark_all(&self, generation: u64) {
        for item in self.iter() {
            item.0.mark_all(generation);
            item.1.mark_all(generation);
        }
    }

    fn unroot(&self) -> Mutability {
        let mut ret = Mutability::None;
        for item in self.iter() {
            ret = item.0.unroot().or(&ret);
            ret = item.1.unroot().or(&ret);
        }
        ret
    }

    fn root(&self) {
        for item in self.iter() {
            item.0.unroot();
            item.1.unroot();
        }
    }

    fn destroy(&self) {
        for item in self.iter() {
            item.0.destroy();
            item.1.destroy();
        }
    }
}

impl<K: Mark, V: Mark> Mark for BTreeMap<K, V> {
    fn mark_all(&self, generation: u64) {
        for item in self.iter() {
            item.0.mark_all(generation);
            item.1.mark_all(generation);
        }
    }

    fn unroot(&self) -> Mutability {
        let mut ret = Mutability::None;
        for item in self.iter() {
            ret = item.0.unroot().or(&ret);
            ret = item.1.unroot().or(&ret);
        }
        ret
    }

    fn root(&self) {
        for item in self.iter() {
            item.0.unroot();
            item.1.unroot();
        }
    }

    fn destroy(&self) {
        for item in self.iter() {
            item.0.destroy();
            item.1.destroy();
        }
    }
}
macro_rules! empty_mark {
    ($($T:ty),*) => {
        $(
            impl Mark for $T {
                fn mark_all(&self, _generation: u64) {}

                fn unroot(&self) -> Mutability {
                    Mutability::None
                }

                fn root(&self) {}

                fn destroy(&self) {}
            }
        )*
    }
}

macro_rules! array_mark_impls {
    ($($n:expr),*) => {
        $(
            iter_impl!(&[T; $n]);
        )*
    }
}

array_mark_impls![
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31
];

macro_rules! tuple_mark {
    ($el1:ident, $($el2:ident,)*) => {
        #[allow(non_snake_case)]
        impl <$el1: $crate::gc::Mark, $($el2: $crate::gc::Mark),*> Mark for ($el1, $($el2,)*) {
                fn mark_all(&self, generation: u64) {
                    self.0.mark_all(generation);
                    match self {
                        (_, $($el2, )*) => {
                            $crate::gc::Mark::mark_all(&($($el2, )*), generation);
                        }
                    }
                }

                fn unroot(&self) -> Mutability {
                    let mut ret = self.0.unroot();
                    match self {
                        (_, $($el2, )*) => {
                            ret = ret.or(&($($el2, )*).unroot());
                        }
                    }
                    return ret;
                }

                fn root(&self) {
                    self.0.root();
                    match self {
                        (_, $($el2, )*) => {
                            ($($el2, )*).unroot();
                        }
                    }
                }

                fn destroy(&self) {
                    self.0.destroy();
                    match self {
                        (_, $($el2, )*) => {
                            ($($el2, )*).destroy();
                        }
                    }
                }
            }
    };
}

macro_rules! tuple_marks {
    [$(($el1:ident, $($el2:ident,)*);)*] => {
        $(
            tuple_mark! {$el1, $($el2,)*}
        )*
    }
}

tuple_marks![
    (A,);
    (A, B,);
    (A, B, C,);
    (A, B, C, D,);
    (A, B, C, D, E,);
    (A, B, C, D, E, F,);
    (A, B, C, D, E, F, G,);
    (A, B, C, D, E, F, G, H,);
    (A, B, C, D, E, F, G, H, I,);
    (A, B, C, D, E, F, G, H, I, J,);
    (A, B, C, D, E, F, G, H, I, J, K,);
    (A, B, C, D, E, F, G, H, I, J, K, L,);
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
