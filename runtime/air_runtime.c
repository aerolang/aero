#include "air_runtime.h"
#include <stdio.h>

void air_log(const char* msg) {
    if (msg) {
        printf("%s\n", msg);
    }
}
