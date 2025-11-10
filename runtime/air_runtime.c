#include "air_runtime.h"
#include <stdio.h>

void air_log(const char* msg) {
    if (msg) {
        printf("%s\n", msg);
    }
}

// Forward declare the AIR main function (compiled as air_main)
extern void air_main(void);

// Provide the C main wrapper that calls air_main and returns 0
int main(void) {
    air_main();
    return 0;
}
