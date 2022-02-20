use super::atom2::Object;
use once_cell::unsync::Lazy;
use std::alloc as sal;

static mut ALLOCATIONS: Lazy<Vec<*mut Object>> = Lazy::new(Vec::new);
const OBJ_LAYOUT: sal::Layout = sal::Layout::new::<Object>();
pub fn alloc(obj: Object) -> *mut Object {
    unsafe {
        let ptr = sal::alloc(OBJ_LAYOUT) as *mut Object;
        ptr.write(obj);
        ALLOCATIONS.push(ptr);

        ptr
    }
}

pub fn sweep() {
    unsafe {
        ALLOCATIONS.retain(|ptr| {
            let mark = (**ptr).mark;
            (**ptr).mark = false;
            if !mark {
                std::ptr::drop_in_place(*ptr);
                sal::dealloc(*ptr as _, OBJ_LAYOUT);
            }
            mark
        })
    }
}
