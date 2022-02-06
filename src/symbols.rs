use std::{cmp, fmt, ptr, str};
use std::alloc::{Layout, handle_alloc_error};
use std::borrow::Borrow;
use std::cell::UnsafeCell;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;

use quickdry::Arena;

/// An arena to intern byte strings, with some value attached.
pub struct SymbolMap<'u, T: 'u> {
    map: UnsafeCell<HashSet<ThinRef<'u, Entry<T>>>>,
    arena: Arena,
}

#[repr(C)]
struct Entry<T> { value: T, key: [u8] }

/// An interned byte string with address-based equality.
#[repr(transparent)]
pub struct Symbol<'i, T> { entry: ThinPtr<Entry<T>>, _marker: PhantomData<&'i Entry<T>> }

impl<T> PartialEq for Entry<T> {
    fn eq(&self, other: &Self) -> bool { <[u8]>::eq(&self.key, &other.key) }
}
impl<T> Eq for Entry<T> {}
impl<T> Hash for Entry<T> { fn hash<H: Hasher>(&self, state: &mut H) { self.key.hash(state); } }
impl<T> Borrow<[u8]> for ThinRef<'_, Entry<T>> { fn borrow(&self) -> &[u8] { &self.key[..] } }

impl<'i, T> Copy for Symbol<'i, T> {}
impl<'i, T> Clone for Symbol<'i, T> { fn clone(&self) -> Self { *self } }
impl<'i, T> PartialEq for Symbol<'i, T> {
    fn eq(&self, other: &Self) -> bool { ThinPtr::eq(&self.entry, &other.entry) }
}
impl<'i, T> Eq for Symbol<'i, T> {}
impl<'i, T> Hash for Symbol<'i, T> {
    fn hash<H: Hasher>(&self, state: &mut H) { ThinPtr::hash(&self.entry, state) }
}

impl<'u, T> Default for SymbolMap<'u, T> {
    fn default() -> SymbolMap<'u, T> {
        let map = UnsafeCell::new(HashSet::default());
        let arena = Arena::default();
        SymbolMap { map, arena }
    }
}

impl<'u, T> SymbolMap<'u, T> {
    pub fn intern(&self, key: &[u8], value: T) -> Symbol<'_, T> {
        unsafe {
            let map = &mut *self.map.get();
            let entry = if let Some(&entry) = map.get(key) {
                entry
            } else {
                let layout = Layout::from_size_align(0, 1).unwrap();
                let (layout, _) = layout.extend(Layout::new::<usize>()).unwrap();
                let (layout, _) = layout.extend(Layout::new::<T>()).unwrap();
                let (layout, _) = layout.extend(Layout::for_value(key)).unwrap();

                let data = self.arena.alloc(layout);
                if data.is_null() { handle_alloc_error(layout); }

                let len = key.len();
                let entry = ptr::slice_from_raw_parts_mut(data, len) as *mut Wide<Entry<T>>;
                ptr::write(ptr::addr_of_mut!((*entry).meta), len);
                ptr::write(ptr::addr_of_mut!((*entry).data.value), value);
                let dst = ptr::addr_of_mut!((*entry).data.key) as *mut u8;
                ptr::copy_nonoverlapping(key.as_ptr(), dst, len);

                let ptr = ptr::NonNull::new_unchecked(data as *mut ());
                let ptr = ThinPtr { ptr, _marker: PhantomData };
                let entry = ThinRef { ptr, _marker: PhantomData };
                map.insert(entry);

                entry
            };

            Symbol { entry: entry.ptr, _marker: PhantomData }
        }
    }

    pub fn len(&self) -> usize {
        unsafe {
            let map = &*self.map.get();
            map.len()
        }
    }
}

impl<'i, T> Symbol<'i, T> {
    pub fn key(self) -> &'i [u8] { &self.entry().key[..] }
    pub fn value(self) -> &'i T { &self.entry().value }
    fn entry(self) -> &'i Entry<T> { unsafe { self.entry.as_ptr().as_ref() } }
}

impl<'a, T: fmt::Debug> fmt::Debug for Symbol<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut key = self.key();
        loop {
            match str::from_utf8(key) {
                Ok(key) => {
                    f.write_str(key)?;
                    break;
                }
                Err(error) => {
                    let (valid, rest) = key.split_at(error.valid_up_to());
                    f.write_str(unsafe { std::str::from_utf8_unchecked(valid) })?;
                    f.write_str("\u{FFFD}")?;
                    key = match error.error_len() {
                        Some(len) => { &rest[len..] }
                        None => { break; }
                    };
                }
            }
        }
        write!(f, "={:?}", self.value())?;
        Ok(())
    }
}

#[repr(transparent)]
pub struct ThinRef<'a, T: ?Sized> { ptr: ThinPtr<T>, _marker: PhantomData<&'a T> }

unsafe impl<'a, T: ?Sized + Sync> Send for ThinRef<'a, T> {}
unsafe impl<'a, T: ?Sized + Sync> Sync for ThinRef<'a, T> {}
impl<'a, T: ?Sized> Copy for ThinRef<'a, T> {}
impl<'a, T: ?Sized> Clone for ThinRef<'a, T> { fn clone(&self) -> Self { *self } }
impl<'a, T: ?Sized> Deref for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {
    type Target = T;
    fn deref(&self) -> &T { unsafe { self.ptr.as_ptr().as_ref() } }
}
impl<'a, T: ?Sized> Borrow<T> for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {
    fn borrow(&self) -> &T { self }
}
impl<'a, T: ?Sized + PartialEq> PartialEq for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {
    fn eq(&self, other: &Self) -> bool { T::eq(self, other) }
}
impl<'a, T: ?Sized + Eq> Eq for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {}
impl<'a, T: ?Sized + PartialOrd> PartialOrd for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> { T::partial_cmp(self, other) }
}
impl<'a, T: ?Sized + Ord> Ord for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering { T::cmp(self, other) }
}
impl<'a, T: ?Sized + Hash> Hash for ThinRef<'a, T> where ThinPtr<T>: AsPtr<Target = T> {
    fn hash<H: Hasher>(&self, state: &mut H) { T::hash(self, state) }
}

#[repr(transparent)]
pub struct ThinPtr<T: ?Sized> { ptr: ptr::NonNull<()>, _marker: PhantomData<*const T> }

impl<T: ?Sized> Copy for ThinPtr<T> {}
impl<T: ?Sized> Clone for ThinPtr<T> { fn clone(&self) -> Self { *self } }
impl<T: ?Sized> fmt::Pointer for ThinPtr<T> where Self: AsPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ptr::NonNull::fmt(&self.as_ptr(), f)
    }
}
impl<T: ?Sized> PartialEq for ThinPtr<T> where Self: AsPtr {
    fn eq(&self, other: &Self) -> bool { ptr::NonNull::eq(&self.as_ptr(), &other.as_ptr()) }
}
impl<T: ?Sized> Eq for ThinPtr<T> where Self: AsPtr {}
impl<T: ?Sized> PartialOrd for ThinPtr<T> where Self: AsPtr {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        ptr::NonNull::partial_cmp(&self.as_ptr(), &other.as_ptr())
    }
}
impl<T: ?Sized> Ord for ThinPtr<T> where Self: AsPtr {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        ptr::NonNull::cmp(&self.as_ptr(), &other.as_ptr())
    }
}
impl<T: ?Sized> Hash for ThinPtr<T> where Self: AsPtr {
    fn hash<H: Hasher>(&self, state: &mut H) { ptr::NonNull::hash(&self.as_ptr(), state) }
}

#[repr(C)]
struct Wide<T: ?Sized + Pointee> { meta: T::Metadata, data: T }

// Temporary substitute for a single implementation using ptr::from_raw_parts_mut.
trait AsPtr {
    type Target: ?Sized;
    fn as_ptr(self) -> ptr::NonNull<Self::Target>;
}
impl<T> AsPtr for ThinPtr<T> {
    type Target = T;
    fn as_ptr(self) -> ptr::NonNull<T> {
        unsafe {
            let data = self.ptr.as_ptr();
            let _meta = ptr::read(data as *const ());
            let wide = data as *mut Wide<T>;
            ptr::NonNull::new_unchecked(ptr::addr_of_mut!((*wide).data))
        }
    }
}
impl<T> AsPtr for ThinPtr<[T]> {
    type Target = [T];
    fn as_ptr(self) -> ptr::NonNull<[T]> {
        unsafe {
            let data = self.ptr.as_ptr();
            let meta = ptr::read(data as *const usize);
            let wide = ptr::slice_from_raw_parts_mut(data, meta) as *mut Wide<[T]>;
            ptr::NonNull::new_unchecked(ptr::addr_of_mut!((*wide).data))
        }
    }
}
impl<T> AsPtr for ThinPtr<Entry<T>> {
    type Target = Entry<T>;
    fn as_ptr(self) -> ptr::NonNull<Entry<T>> {
        unsafe {
            let data = self.ptr.as_ptr();
            let meta = ptr::read(data as *const usize);
            let wide = ptr::slice_from_raw_parts_mut(data, meta) as *mut Wide<Entry<T>>;
            ptr::NonNull::new_unchecked(ptr::addr_of_mut!((*wide).data))
        }
    }
}

// Temporary substitute for ptr::Pointee.
trait Pointee { type Metadata: Copy + Send + Sync + Ord + Hash + Unpin; }
impl<T> Pointee for T { type Metadata = (); }
impl<T> Pointee for [T] { type Metadata = usize; }
impl<T> Pointee for Entry<T> { type Metadata = usize; }

#[cfg(test)]
mod tests {
    use super::SymbolMap;

    #[test]
    fn map() {
        let map = SymbolMap::default();

        let foo = map.intern(b"foo", 3);
        let bar = map.intern(b"bar", 5);
        let baz = map.intern(b"baz", 7);
        let quux = map.intern(b"quux", 9);

        assert_eq!(foo.key(), b"foo");
        assert_eq!(*foo.value(), 3);
        assert_eq!(bar.key(), b"bar");
        assert_eq!(*bar.value(), 5);
        assert_eq!(baz.key(), b"baz");
        assert_eq!(*baz.value(), 7);
        assert_eq!(quux.key(), b"quux");
        assert_eq!(*quux.value(), 9);

        let new = map.intern(b"baz", 13);
        assert_eq!(new, baz);
        assert_eq!(new.key(), b"baz");
        assert_eq!(*new.value(), 7);
    }
}
