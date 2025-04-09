#[cxx::bridge(namespace = "airc::codegen")]
mod ffi {
    unsafe extern "C++" {
        include!("compiler/airc-codegen/include/codegen.h");

        fn do_stuff();
    }
}

pub fn do_stuff() {
    ffi::do_stuff();
}
