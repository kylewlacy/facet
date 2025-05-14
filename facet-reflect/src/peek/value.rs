use core::{cmp::Ordering, marker::PhantomData};
use facet_core::{
    Def, Facet, PointerType, PtrConst, PtrMut, SequenceType, Shape, ShapeAttribute, Type,
    TypeNameOpts, UserType, ValueVTable,
};

use crate::{ReflectError, ScalarType};

use super::{
    ListLikeDef, PeekEnum, PeekList, PeekListLike, PeekMap, PeekSmartPointer, PeekStruct, PeekTuple,
};

/// A unique identifier for a peek value
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId {
    pub(crate) shape: &'static Shape,
    pub(crate) ptr: *const u8,
}

impl ValueId {
    pub(crate) fn new(shape: &'static Shape, ptr: *const u8) -> Self {
        Self { shape, ptr }
    }
}

impl core::fmt::Display for ValueId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}@{:p}", self.shape, self.ptr)
    }
}

impl core::fmt::Debug for ValueId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

/// Lets you read from a value (implements read-only [`ValueVTable`] proxies)
#[derive(Clone, Copy)]
pub struct Peek<'mem, 'facet_lifetime> {
    /// Underlying data
    pub(crate) data: PtrConst<'mem>,

    /// Shape of the value
    pub(crate) shape: &'static Shape,

    invariant: PhantomData<fn(&'facet_lifetime ()) -> &'facet_lifetime ()>,
}

impl<'mem, 'facet_lifetime> Peek<'mem, 'facet_lifetime> {
    /// Creates a new `PeekValue` instance for a value of type `T`.
    pub fn new<T: Facet<'facet_lifetime>>(t: &'mem T) -> Self {
        Self {
            data: PtrConst::new(t as *const T),
            shape: T::SHAPE,
            invariant: PhantomData,
        }
    }

    /// Creates a new `PeekValue` instance without checking the type.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it doesn't check if the provided data
    /// and shape are compatible. The caller must ensure that the data is valid
    /// for the given shape.
    pub unsafe fn unchecked_new(data: PtrConst<'mem>, shape: &'static Shape) -> Self {
        Self {
            data,
            shape,
            invariant: PhantomData,
        }
    }

    /// Returns the vtable
    #[inline(always)]
    pub fn vtable(&self) -> &'static ValueVTable {
        self.shape.vtable
    }

    /// Returns a unique identifier for this value, usable for cycle detection
    pub fn id(&self) -> ValueId {
        ValueId::new(self.shape, self.data.as_byte_ptr())
    }

    /// Returns true if the two values are pointer-equal
    #[inline]
    pub fn ptr_eq(&self, other: &Peek<'_, '_>) -> bool {
        self.data.as_byte_ptr() == other.data.as_byte_ptr()
    }

    /// Returns true if this scalar is equal to the other scalar
    ///
    /// # Returns
    ///
    /// `false` if equality comparison is not supported for this scalar type
    #[inline]
    pub fn eq(&self, other: &Peek<'_, '_>) -> Option<bool> {
        unsafe {
            self.shape
                .vtable
                .eq
                .map(|eq_fn| eq_fn(self.data, other.data))
        }
    }

    /// Compares this scalar with another and returns their ordering
    ///
    /// # Returns
    ///
    /// `None` if comparison is not supported for this scalar type
    #[inline]
    pub fn partial_cmp(&self, other: &Peek<'_, '_>) -> Option<Ordering> {
        unsafe {
            self.shape
                .vtable
                .partial_ord
                .and_then(|partial_ord_fn| partial_ord_fn(self.data, other.data))
        }
    }

    /// Hashes this scalar
    ///
    /// # Returns
    ///
    /// `false` if hashing is not supported for this scalar type, `true` otherwise
    #[inline(always)]
    pub fn hash<H: core::hash::Hasher>(&self, hasher: &mut H) -> bool {
        unsafe {
            if let Some(hash_fn) = self.shape.vtable.hash {
                let hasher_opaque = PtrMut::new(hasher);
                hash_fn(self.data, hasher_opaque, |opaque, bytes| {
                    opaque.as_mut::<H>().write(bytes)
                });
                true
            } else {
                false
            }
        }
    }

    /// Returns the type name of this scalar
    ///
    /// # Arguments
    ///
    /// * `f` - A mutable reference to a `core::fmt::Formatter`
    /// * `opts` - The `TypeNameOpts` to use for formatting
    ///
    /// # Returns
    ///
    /// The result of the type name formatting
    #[inline(always)]
    pub fn type_name(
        &self,
        f: &mut core::fmt::Formatter<'_>,
        opts: TypeNameOpts,
    ) -> core::fmt::Result {
        (self.shape.vtable.type_name)(f, opts)
    }

    /// Returns the shape
    #[inline(always)]
    pub const fn shape(&self) -> &'static Shape {
        self.shape
    }

    /// Returns the data
    #[inline(always)]
    pub const fn data(&self) -> PtrConst<'mem> {
        self.data
    }

    /// Get the scalar type if set.
    pub fn scalar_type(&self) -> Option<ScalarType> {
        ScalarType::try_from_shape(self.shape)
    }

    /// Read the value from memory into a Rust value.
    ///
    /// # Panics
    ///
    /// Panics if the shape doesn't match the type `T`.
    pub fn get<T: Facet<'facet_lifetime>>(&self) -> Result<&T, ReflectError> {
        if self.shape != T::SHAPE {
            Err(ReflectError::WrongShape {
                expected: self.shape,
                actual: T::SHAPE,
            })
        } else {
            Ok(unsafe { self.data.get::<T>() })
        }
    }

    /// Try to get the value as a string if it's a string type
    /// Returns None if the value is not a string or couldn't be extracted
    pub fn as_str(&self) -> Option<&str> {
        let peek = self.innermost_peek();
        if let Some(ScalarType::Str) = peek.scalar_type() {
            unsafe { Some(peek.data.get::<&str>()) }
        } else if let Some(ScalarType::String) = peek.scalar_type() {
            unsafe { Some(peek.data.get::<alloc::string::String>().as_str()) }
        } else if let Type::Pointer(PointerType::Reference(vpt)) = peek.shape.ty {
            let target_shape = (vpt.target)();
            if let Some(ScalarType::Str) = ScalarType::try_from_shape(target_shape) {
                unsafe { Some(peek.data.get::<&str>()) }
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Tries to identify this value as a struct
    pub fn into_struct(self) -> Result<PeekStruct<'mem, 'facet_lifetime>, ReflectError> {
        if let Type::User(UserType::Struct(ty)) = self.shape.ty {
            Ok(PeekStruct { value: self, ty })
        } else {
            Err(ReflectError::WasNotA {
                expected: "struct",
                actual: self.shape,
            })
        }
    }

    /// Tries to identify this value as an enum
    pub fn into_enum(self) -> Result<PeekEnum<'mem, 'facet_lifetime>, ReflectError> {
        if let Type::User(UserType::Enum(ty)) = self.shape.ty {
            Ok(PeekEnum { value: self, ty })
        } else {
            Err(ReflectError::WasNotA {
                expected: "enum",
                actual: self.shape,
            })
        }
    }

    /// Tries to identify this value as a map
    pub fn into_map(self) -> Result<PeekMap<'mem, 'facet_lifetime>, ReflectError> {
        if let Def::Map(def) = self.shape.def {
            Ok(PeekMap { value: self, def })
        } else {
            Err(ReflectError::WasNotA {
                expected: "map",
                actual: self.shape,
            })
        }
    }

    /// Tries to identify this value as a list
    pub fn into_list(self) -> Result<PeekList<'mem, 'facet_lifetime>, ReflectError> {
        if let Def::List(def) = self.shape.def {
            Ok(PeekList { value: self, def })
        } else {
            Err(ReflectError::WasNotA {
                expected: "list",
                actual: self.shape,
            })
        }
    }

    /// Tries to identify this value as a list, array or slice
    pub fn into_list_like(self) -> Result<PeekListLike<'mem, 'facet_lifetime>, ReflectError> {
        match self.shape.def {
            Def::List(def) => Ok(PeekListLike::new(self, ListLikeDef::List(def))),
            Def::Array(def) => Ok(PeekListLike::new(self, ListLikeDef::Array(def))),
            _ => {
                // &[i32] is actually a _pointer_ to a slice.
                match self.shape.ty {
                    Type::Pointer(ptr) => match ptr {
                        PointerType::Reference(vpt) | PointerType::Raw(vpt) => {
                            let target = (vpt.target)();
                            match target.def {
                                Def::Slice(def) => {
                                    return Ok(PeekListLike::new(self, ListLikeDef::Slice(def)));
                                }
                                _ => {
                                    // well it's not list-like then
                                }
                            }
                        }
                        PointerType::Function(_) => {
                            // well that's not a list-like
                        }
                    },
                    _ => {
                        // well that's not a list-like either
                    }
                }

                Err(ReflectError::WasNotA {
                    expected: "list, array or slice",
                    actual: self.shape,
                })
            }
        }
    }

    /// Tries to identify this value as a smart pointer
    pub fn into_smart_pointer(
        self,
    ) -> Result<PeekSmartPointer<'mem, 'facet_lifetime>, ReflectError> {
        if let Def::SmartPointer(def) = self.shape.def {
            Ok(PeekSmartPointer { value: self, def })
        } else {
            Err(ReflectError::WasNotA {
                expected: "smart pointer",
                actual: self.shape,
            })
        }
    }

    /// Tries to identify this value as an option
    pub fn into_option(self) -> Result<super::PeekOption<'mem, 'facet_lifetime>, ReflectError> {
        if let Def::Option(def) = self.shape.def {
            Ok(super::PeekOption { value: self, def })
        } else {
            Err(ReflectError::WasNotA {
                expected: "option",
                actual: self.shape,
            })
        }
    }

    /// Tries to identify this value as a tuple
    pub fn into_tuple(self) -> Result<PeekTuple<'mem, 'facet_lifetime>, ReflectError> {
        if let Type::Sequence(SequenceType::Tuple(ty)) = self.shape.ty {
            Ok(PeekTuple { value: self, ty })
        } else {
            Err(ReflectError::WasNotA {
                expected: "tuple",
                actual: self.shape,
            })
        }
    }

    /// Tries to read the inner value of a transparent wrapper type. For
    /// example, `Box<Vec<u8>>` will get the inner `Vec<u8>` value.
    ///
    /// Returns `Ok(Some(Peek))` with the inner value, `Ok(None)` if this shape
    /// has no inner shape, or `Err(_)` if the inner value could not be
    /// borrowed.
    ///
    /// See also [Self::innermost_peek], which repeats this process until
    /// reaching the innermost value.
    pub fn inner_peek(self) -> Result<Option<Self>, ReflectError> {
        // Resolve wrapper types by traversing into their inner type
        let is_wrapper = matches!(self.shape.def, Def::SmartPointer(_))
            || matches!(self.shape.ty, Type::Pointer(PointerType::Reference(_)));
        if is_wrapper {
            if let (Some(try_borrow_inner_fn), Some(inner_shape)) =
                (self.shape.vtable.try_borrow_inner, self.shape.inner)
            {
                let inner_data = unsafe { try_borrow_inner_fn(self.data) };
                let inner_data = inner_data.map_err(|e| ReflectError::TryBorrowInnerError {
                    shape: self.shape,
                    inner_shape: inner_shape(),
                    inner: e,
                })?;

                return Ok(Some(Peek {
                    data: inner_data,
                    shape: inner_shape(),
                    invariant: PhantomData,
                }));
            }
        }

        // Resolve #[facet(transparent)] attribute to returning the inner field
        let is_transparent = self
            .shape
            .attributes
            .iter()
            .any(|attr| *attr == ShapeAttribute::Transparent);
        if is_transparent {
            let peek_struct = self
                .into_struct()
                .map_err(|_| ReflectError::InvariantViolation {
                    invariant: "shape has #[facet(transparent)] attr but is not a struct",
                })?;
            let inner = peek_struct.field(0).map_err(|e| ReflectError::FieldError {
                shape: self.shape,
                field_error: e,
            })?;
            return Ok(Some(inner));
        }

        Ok(None)
    }

    /// Tries to return the innermost value — useful for serialization. For example, we serialize a `NonZero<u8>` the same
    /// as a `u8`. Similarly, we serialize a `Utf8PathBuf` the same as a `String.
    ///
    /// Returns a `Peek` to the innermost value, unwrapping transparent wrappers recursively.
    /// For example, this will peel through newtype wrappers or smart pointers that have an `inner`.
    pub fn innermost_peek(self) -> Self {
        let mut current_peek = self;
        loop {
            let inner = current_peek.inner_peek().unwrap_or_else(|e| {
                panic!("innermost_peek: error while trying to get the inner value. error: {e}")
            });
            let Some(inner) = inner else {
                // We reached the innermost type, so we're done
                return current_peek;
            };

            current_peek = inner;
        }
    }
}

impl core::fmt::Display for Peek<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(display_fn) = self.vtable().display {
            unsafe { display_fn(self.data, f) }
        } else {
            write!(f, "⟨{}⟩", self.shape)
        }
    }
}

impl core::fmt::Debug for Peek<'_, '_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(debug_fn) = self.vtable().debug {
            unsafe { debug_fn(self.data, f) }
        } else {
            write!(f, "⟨{}⟩", self.shape)
        }
    }
}

impl core::cmp::PartialEq for Peek<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        if self.shape != other.shape {
            return false;
        }
        let eq_fn = match self.shape.vtable.eq {
            Some(eq_fn) => eq_fn,
            None => return false,
        };
        unsafe { eq_fn(self.data, other.data) }
    }
}

impl core::cmp::PartialOrd for Peek<'_, '_> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        if self.shape != other.shape {
            return None;
        }
        let partial_ord_fn = self.shape.vtable.partial_ord?;
        unsafe { partial_ord_fn(self.data, other.data) }
    }
}

impl core::hash::Hash for Peek<'_, '_> {
    fn hash<H: core::hash::Hasher>(&self, hasher: &mut H) {
        if let Some(hash_fn) = self.shape.vtable.hash {
            let hasher_opaque = PtrMut::new(hasher);
            unsafe {
                hash_fn(self.data, hasher_opaque, |opaque, bytes| {
                    opaque.as_mut::<H>().write(bytes)
                })
            };
        } else {
            panic!("Hashing is not supported for this shape");
        }
    }
}
