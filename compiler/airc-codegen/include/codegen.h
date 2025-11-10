#ifndef AIRC_CODEGEN_CODEGEN_H_
#define AIRC_CODEGEN_CODEGEN_H_

// Forward declare rust::Str from cxx.h (will be defined by cxxbridge generated code)
namespace rust {
inline namespace cxxbridge1 {
class Str;
}
}

namespace airc {
namespace codegen {

void do_stuff();

// Compile AIR source to an executable
void compile_air(rust::Str source, rust::Str output_path);

}  // namespace codegen
}  // namespace airc

#endif  // AIRC_CODEGEN_CODEGEN_H_
