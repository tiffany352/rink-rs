use std::{
    alloc::{GlobalAlloc, Layout, System},
    ptr,
    sync::atomic::{AtomicUsize, Ordering},
};

pub struct RinkAlloc {
    system: System,
    used: AtomicUsize,
    max: AtomicUsize,
    limit: AtomicUsize,
}

impl RinkAlloc {
    pub const fn new(system: System, limit: usize) -> RinkAlloc {
        RinkAlloc {
            system,
            used: AtomicUsize::new(0),
            max: AtomicUsize::new(0),
            limit: AtomicUsize::new(limit),
        }
    }

    pub fn reset_max(&self) {
        self.max
            .store(self.used.load(Ordering::Acquire), Ordering::Release);
    }

    pub fn get_max(&self) -> usize {
        self.max.load(Ordering::Acquire)
    }

    pub fn set_limit(&self, limit: usize) {
        self.limit.store(limit, Ordering::Release);
    }
}

unsafe impl GlobalAlloc for RinkAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let limit = self.limit.load(Ordering::Acquire);
        let new_size = self.used.fetch_add(size, Ordering::Acquire) + size;
        if new_size <= limit {
            self.max.fetch_max(new_size, Ordering::Relaxed);
            let result = self.system.alloc(layout);
            if result.is_null() {
                self.used.fetch_sub(size, Ordering::Release);
            }
            result
        } else {
            self.used.fetch_sub(size, Ordering::Release);
            ptr::null_mut()
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let size = layout.size();
        self.system.dealloc(ptr, layout);
        self.used.fetch_sub(size, Ordering::Release);
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let limit = self.limit.load(Ordering::Acquire);
        let new_size = self.used.fetch_add(size, Ordering::Acquire) + size;
        if new_size <= limit {
            self.max.fetch_max(new_size, Ordering::Relaxed);
            let result = self.system.alloc_zeroed(layout);
            if result.is_null() {
                self.used.fetch_sub(size, Ordering::Release);
            }
            result
        } else {
            self.used.fetch_sub(size, Ordering::Release);
            ptr::null_mut()
        }
    }

    unsafe fn realloc(&self, ptr: *mut u8, old_layout: Layout, realloc_size: usize) -> *mut u8 {
        let new_layout = Layout::from_size_align_unchecked(realloc_size, old_layout.align());
        let (old_size, new_size) = (old_layout.size(), new_layout.size());

        let limit = self.limit.load(Ordering::Acquire);
        let new_used = self.used.fetch_add(new_size, Ordering::Acquire) + new_size;
        if new_used <= limit {
            let result = self.system.realloc(ptr, old_layout, realloc_size);
            if result.is_null() {
                self.used.fetch_sub(new_size, Ordering::Release);
            } else {
                self.used.fetch_sub(old_size, Ordering::Release);
            }
            result
        } else {
            self.used.fetch_sub(new_size, Ordering::Release);
            ptr::null_mut()
        }
    }
}
