use std::{
    alloc::{GlobalAlloc, System},
    env::args,
    sync::atomic::{AtomicUsize, Ordering},
};

struct Alloc {
    max_used: AtomicUsize,
    system: System,
}

unsafe impl GlobalAlloc for Alloc {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        self.max_used.fetch_add(layout.size(), Ordering::Acquire);
        self.system.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        self.max_used.fetch_sub(layout.size(), Ordering::Release);
        self.system.dealloc(ptr, layout)
    }
}

#[global_allocator]
static ALLOCATOR: Alloc = Alloc {
    max_used: AtomicUsize::new(0),
    system: System,
};

fn main() {
    let start = ALLOCATOR.max_used.load(Ordering::SeqCst);
    let data = rink_data::load_data();
    let stop = ALLOCATOR.max_used.load(Ordering::SeqCst);

    let arg = args().nth(1);
    match arg.as_ref().map(|x| &x[..]) {
        Some("quantities") => println!("{:?}", data.quantities),
        Some(_) => println!("invalid argument"),
        None => println!("{:?}", data),
    }
    println!("size: {} bytes", rink_data::DATA.len());
    println!(
        "memory: {} kB ({} kB more than start)",
        stop / 1000,
        (stop - start) / 1000
    );
}
