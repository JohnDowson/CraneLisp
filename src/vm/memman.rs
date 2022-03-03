use cl_alloc::round_up;
use cl_alloc::CLAlloc;
use cl_alloc::ObjectHeader;
use cl_alloc::CELL_SIZE;
use once_cell::unsync::Lazy;
use somok::Somok;
use std::ptr::NonNull;

use super::atom::LispType;
use super::atom::Object;
use super::atom::Type;

static mut ALLOCATOR: Lazy<CLAlloc> = Lazy::new(CLAlloc::new);
pub fn allocate_raw(ty: Type) -> Option<NonNull<ObjectHeader>> {
    let size = round_up(ty.size(), CELL_SIZE);
    let run_size = (size / CELL_SIZE) + 1;
    unsafe {
        let ptr = ALLOCATOR.alloc(run_size)?;

        let ptr: *mut ObjectHeader = ptr.as_ptr().cast();
        ptr.write(ObjectHeader::new(ty as usize, size as u32, run_size as u32));
        NonNull::new_unchecked(ptr).some()
    }
}
pub fn allocate<T: LispType + 'static>(obj: T) -> Option<Object<T>> {
    let ptr = allocate_raw(T::type_tag())?;
    unsafe {
        let objp = ptr.as_ptr().add(1).cast::<T>();
        objp.write(obj);
        Object::from_ptr(objp).some()
    }
}
pub fn allocate_raw_and_store<T: LispType + 'static>(obj: T) -> Option<NonNull<ObjectHeader>> {
    let ptr = allocate_raw(T::type_tag())?;
    unsafe {
        let objp = ptr.as_ptr().add(1).cast::<T>();
        objp.write(obj);
        ptr.some()
    }
}

pub fn mark(ptr: *const u8) -> bool {
    unsafe { ALLOCATOR.mark(ptr) }
}
pub fn sweep() {
    unsafe { ALLOCATOR.sweep() }
}
