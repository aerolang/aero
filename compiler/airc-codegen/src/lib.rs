#[cxx::bridge(namespace = "airc::codegen")]
mod ffi {
    unsafe extern "C++" {
        include!("compiler/airc-codegen/include/codegen.h");

        fn do_stuff();
        fn compile_air(source: &str, output_path: &str, runtime_path: &str);
    }
}

pub fn do_stuff() {
    ffi::do_stuff();
}

pub fn compile_air(source: &str, output_path: &str, runtime_path: &str) {
    ffi::compile_air(source, output_path, runtime_path);
}
