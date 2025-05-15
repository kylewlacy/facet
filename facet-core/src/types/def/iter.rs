use crate::{PtrConst, PtrMut};

/// Create a new iterator that iterates over the provided value
///
/// # Safety
///
/// The `value` parameter must point to aligned, initialized memory of the correct type.
pub type IterNewFn = for<'value> unsafe fn(value: PtrConst<'value>) -> PtrMut<'value>;

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
    pub new: Option<IterNewFn>,

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
