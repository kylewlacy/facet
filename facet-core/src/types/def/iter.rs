use crate::{PtrConst, PtrMut};

/// Create a new iterator that iterates over the provided value
///
/// # Safety
///
/// The `value` parameter must point to aligned, initialized memory of the correct type.
pub type IterInitWithValueFn = for<'value> unsafe fn(value: PtrConst<'value>) -> PtrMut<'value>;

/// Advance the iterator, returning the next value from the iterator
///
/// # Safety
///
/// The `iter` parameter must point to aligned, initialized memory of the correct type.
pub type IterNextFn = for<'iter> unsafe fn(iter: PtrMut<'iter>) -> Option<PtrConst<'iter>>;

/// Advance the iterator, returning the next value pair from the iterator.
/// For example, this would return the next key/value pair from a map.
///
/// # Safety
///
/// The `iter` parameter must point to aligned, initialized memory of the correct type.
pub type IterNextPairFn =
    for<'iter> unsafe fn(iter: PtrMut<'iter>) -> Option<(PtrConst<'iter>, PtrConst<'iter>)>;

/// Advance the iterator in reverse, returning the next value from the end
/// of the iterator.
///
/// # Safety
///
/// The `iter` parameter must point to aligned, initialized memory of the correct type.
pub type IterNextBackFn = for<'iter> unsafe fn(iter: PtrMut<'iter>) -> Option<PtrConst<'iter>>;

/// Advance the iterator in reverse, returning the next value pair from the
/// end of the iterator. For example, this would return the end key/value
/// pair from a map.
///
/// # Safety
///
/// The `iter` parameter must point to aligned, initialized memory of the correct type.
pub type IterNextPairBackFn =
    for<'iter> unsafe fn(iter: PtrMut<'iter>) -> Option<(PtrConst<'iter>, PtrConst<'iter>)>;

/// Return the lower and upper bounds of the iterator, if known.
///
/// # Safety
///
/// The `iter` parameter must point to aligned, initialized memory of the correct type.
pub type IterSizeHintFn =
    for<'iter> unsafe fn(iter: PtrMut<'iter>) -> Option<(usize, Option<usize>)>;

/// Deallocate the iterator
///
/// # Safety
///
/// The `iter` parameter must point to aligned, initialized memory of the correct type.
pub type IterDeallocFn = for<'iter> unsafe fn(iter: PtrMut<'iter>);

/// VTable for an iterator
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(C)]
#[non_exhaustive]
pub struct IterVTable {
    /// cf. [`IterInitWithValueFn`]
    pub init_with_value: Option<IterInitWithValueFn>,

    /// cf. [`IterNextFn`]
    pub next: IterNextFn,

    /// cf. [`IterNextPairFn`]
    pub next_pair: Option<IterNextPairFn>,

    /// cf. [`IterNextBackFn`]
    pub next_back: Option<IterNextBackFn>,

    /// cf. [`IterNextPairBackFn`]
    pub next_pair_back: Option<IterNextPairBackFn>,

    /// cf. [`IterSizeHintFn`]
    pub size_hint: Option<IterSizeHintFn>,

    /// cf. [`IterDeallocFn`]
    pub dealloc: IterDeallocFn,
}

impl IterVTable {
    /// Returns a builder for [`IterVTable`]
    pub const fn builder() -> IterVTableBuilder {
        IterVTableBuilder::new()
    }
}

/// Builds an [`IterVTable`]
pub struct IterVTableBuilder {
    init_with_value: Option<IterInitWithValueFn>,
    next: Option<IterNextFn>,
    next_pair: Option<IterNextPairFn>,
    next_back: Option<IterNextBackFn>,
    next_pair_back: Option<IterNextPairBackFn>,
    size_hint: Option<IterSizeHintFn>,
    dealloc: Option<IterDeallocFn>,
}

impl IterVTableBuilder {
    /// Creates a new [`IterVTableBuilder`] with all fields set to `None`.
    #[allow(clippy::new_without_default)]
    pub const fn new() -> Self {
        Self {
            init_with_value: None,
            next: None,
            next_pair: None,
            next_back: None,
            next_pair_back: None,
            size_hint: None,
            dealloc: None,
        }
    }

    /// Sets the `init_with_value` function
    pub const fn init_with_value(mut self, f: IterInitWithValueFn) -> Self {
        self.init_with_value = Some(f);
        self
    }

    /// Sets the `next` function
    pub const fn next(mut self, f: IterNextFn) -> Self {
        self.next = Some(f);
        self
    }

    /// Sets the `next_pair` function
    pub const fn next_pair(mut self, f: IterNextPairFn) -> Self {
        self.next_pair = Some(f);
        self
    }

    /// Sets the `next_back` function
    pub const fn next_back(mut self, f: IterNextBackFn) -> Self {
        self.next_back = Some(f);
        self
    }

    /// Sets the `next_pair_back` function
    pub const fn next_pair_back(mut self, f: IterNextPairBackFn) -> Self {
        self.next_pair_back = Some(f);
        self
    }

    /// Sets the `dealloc` function
    pub const fn dealloc(mut self, f: IterDeallocFn) -> Self {
        self.dealloc = Some(f);
        self
    }

    /// Builds the [`IterVTable`] from the current state of the builder.
    ///
    /// # Panics
    ///
    /// This method will panic if any of the required fields are `None`.
    pub const fn build(self) -> IterVTable {
        IterVTable {
            init_with_value: self.init_with_value,
            next: self.next.unwrap(),
            next_pair: self.next_pair,
            next_back: self.next_back,
            next_pair_back: self.next_pair_back,
            size_hint: self.size_hint,
            dealloc: self.dealloc.unwrap(),
        }
    }
}
