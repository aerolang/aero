#ifndef AIRC_CODEGEN_CODEGEN_H_
#define AIRC_CODEGEN_CODEGEN_H_

#include <cstdint>

namespace rust {
inline namespace cxxbridge1 {
class Str;
template <typename T> class Vec;
template <typename T> class Slice;
} // namespace cxxbridge1
} // namespace rust

namespace airc::codegen {

// Forward-declared from the cxxbridge-generated header.
struct Diag;
struct CompileResult;

CompileResult compile_air_bytes(rust::Slice<const uint8_t> msg,
                                rust::Str output_path, rust::Str support_path);

} // namespace airc::codegen

#endif // AIRC_CODEGEN_CODEGEN_H_
