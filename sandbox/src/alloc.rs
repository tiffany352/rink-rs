// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Wraps an allocator, adding memory use limits and tracking for peak
/// memory usage.
pub struct Alloc<A = System> {
    parent: A,
    used: AtomicUsize,
    peak: AtomicUsize,
    limit: AtomicUsize,
}

impl Alloc<System> {
    /// Creates a new wrapper allocator using the system allocator.
    pub const fn new(limit: usize) -> Alloc<System> {
        Alloc {
            parent: System,
            used: AtomicUsize::new(0),
            peak: AtomicUsize::new(0),
            limit: AtomicUsize::new(limit),
        }
    }
}

impl<A> Alloc<A>
where
    A: GlobalAlloc,
{
    /// Creates a new wrapper allocator using any GlobalAllocator.
    ///
    /// Unfortunately, this can't be a const fn, because trait bounds on const fns are unstable.
    /// <https://github.com/rust-lang/rust/issues/57563>
    pub fn new_with(parent: A, limit: usize) -> Alloc<A> {
        Alloc {
            parent,
            used: AtomicUsize::new(0),
            peak: AtomicUsize::new(0),
            limit: AtomicUsize::new(limit),
        }
    }

    /// Clears the current peak value.
    pub fn clear_peak(&self) {
        self.peak
            .store(self.used.load(Ordering::Acquire), Ordering::Release);
    }

    /// Returns the peak memory that's been used since startup or since
    /// `reset_max()` was called.
    pub fn get_peak(&self) -> usize {
        self.peak.load(Ordering::Acquire)
    }

    /// Sets the maximum amount of memory that can be used. This should
    /// be called early in the application lifecycle.
    pub fn set_limit(&self, limit: usize) {
        self.limit.store(limit, Ordering::Release);
    }
}

unsafe impl GlobalAlloc for Alloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let limit = self.limit.load(Ordering::Acquire);
        let new_size = self.used.fetch_add(size, Ordering::Acquire) + size;
        if new_size <= limit {
            self.peak.fetch_max(new_size, Ordering::Relaxed);
            let result = self.parent.alloc(layout);
            if result.is_null() {
                self.used.fetch_sub(size, Ordering::Release);
            }
            result
        } else {
            self.used.fetch_sub(size, Ordering::Release);
            std::ptr::null_mut()
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let size = layout.size();
        self.parent.dealloc(ptr, layout);
        self.used.fetch_sub(size, Ordering::Release);
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let limit = self.limit.load(Ordering::Acquire);
        let new_size = self.used.fetch_add(size, Ordering::Acquire) + size;
        if new_size <= limit {
            self.peak.fetch_max(new_size, Ordering::Relaxed);
            let result = self.parent.alloc_zeroed(layout);
            if result.is_null() {
                self.used.fetch_sub(size, Ordering::Release);
            }
            result
        } else {
            self.used.fetch_sub(size, Ordering::Release);
            std::ptr::null_mut()
        }
    }

    unsafe fn realloc(&self, ptr: *mut u8, old_layout: Layout, realloc_size: usize) -> *mut u8 {
        let new_layout = Layout::from_size_align_unchecked(realloc_size, old_layout.align());
        let (old_size, new_size) = (old_layout.size(), new_layout.size());

        let limit = self.limit.load(Ordering::Acquire);
        let new_used = self.used.fetch_add(new_size, Ordering::Acquire) + new_size;
        if new_used <= limit {
            let result = self.parent.realloc(ptr, old_layout, realloc_size);
            if result.is_null() {
                self.used.fetch_sub(new_size, Ordering::Release);
            } else {
                self.used.fetch_sub(old_size, Ordering::Release);
            }
            result
        } else {
            self.used.fetch_sub(new_size, Ordering::Release);
            std::ptr::null_mut()
        }
    }
}
