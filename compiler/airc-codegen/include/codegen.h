#ifndef AIRC_CODEGEN_CODEGEN_H_
#define AIRC_CODEGEN_CODEGEN_H_

// Forward declare rust types from cxx.h (will be defined by cxxbridge generated code)
namespace rust {
inline namespace cxxbridge1 {
class Str;
template<typename T>
class Vec;
}
}

namespace airc {
namespace codegen {

// Forward declare FuncInfo (will be defined by cxxbridge)
struct FuncInfo;

// Compile AIR AST to an executable
void compile_air_ast(rust::Vec<FuncInfo> funcs, rust::Str output_path, rust::Str runtime_path);

}  // namespace codegen
}  // namespace airc

#endif  // AIRC_CODEGEN_CODEGEN_H_
