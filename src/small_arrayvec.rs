//! An adaptation of `arrayvec::ArrayVec` which uses a single byte for the length.
//! The code in this module is primarily derived from the implementations of `Vec` and `ArrayVec`.
use core::slice;
use std::fmt;
use std::iter;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::ptr;

pub struct SmallArrayVec<T, const CAP: usize> {
    buf: [MaybeUninit<T>; CAP],
    len: u8,
}

impl<T, const CAP: usize> Drop for SmallArrayVec<T, CAP> {
    fn drop(&mut self) {
        self.clear();

        // MaybeUninit inhibits array's drop
    }
}

impl<T, const CAP: usize> Deref for SmallArrayVec<T, CAP> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.as_ptr(), self.len as usize) }
    }
}

impl<T, const CAP: usize> DerefMut for SmallArrayVec<T, CAP> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.as_mut_ptr(), self.len as usize) }
    }
}

impl<T: fmt::Debug, const CAP: usize> fmt::Debug for SmallArrayVec<T, CAP> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: Clone, const CAP: usize> Clone for SmallArrayVec<T, CAP> {
    fn clone(&self) -> Self {
        self.iter().cloned().collect()
    }
}

impl<T: Clone, const CAP: usize> iter::FromIterator<T> for SmallArrayVec<T, CAP> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut out = Self::new();
        for elem in iter {
            out.push(elem);
        }
        out
    }
}

impl<T, const CAP: usize> Default for SmallArrayVec<T, CAP> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const CAP: usize> SmallArrayVec<T, CAP> {
    pub fn new() -> Self {
        Self {
            // SAFETY: An uninitialized `[MaybeUninit<_>; LEN]` is valid.
            buf: unsafe { MaybeUninit::uninit().assume_init() },
            len: 0,
        }
    }

    pub fn as_ptr(&self) -> *const T {
        // We shadow the slice method of the same name to avoid going through
        // `deref`, which creates an intermediate reference.
        self.buf.as_ptr() as *const T
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        // We shadow the slice method of the same name to avoid going through
        // `deref`, which creates an intermediate reference.
        self.buf.as_mut_ptr() as *mut T
    }

    pub fn push(&mut self, elem: T) {
        assert!(self.len < u8::MAX);
        let pos = self.len as usize;
        self.buf[pos].write(elem);
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        match self.len.checked_sub(1) {
            None => None,
            Some(new_len) => {
                self.len = new_len;
                unsafe { Some(self.buf[new_len as usize].assume_init_read()) }
            }
        }
    }

    pub fn clear(&mut self) {
        let elems: *mut [T] = self.as_mut_slice();

        // SAFETY:
        // - `elems` comes directly from `as_mut_slice` and is therefore valid.
        // - Setting `self.len` before calling `drop_in_place` means that,
        //   if an element's `Drop` impl panics, the vector's `Drop` impl will
        //   do nothing (leaking the rest of the elements) instead of dropping
        //   some twice.
        unsafe {
            self.len = 0;
            ptr::drop_in_place(elems);
        }
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self
    }
}
