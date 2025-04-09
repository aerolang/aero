#include <iostream>

#include "compiler/airc-codegen/include/air-ast.h"

#include "compiler/airc-codegen/include/codegen.h"

namespace airc {
namespace codegen {

void do_stuff() { std::cout << get_message() << std::endl; }

}  // namespace codegen
}  // namespace airc
