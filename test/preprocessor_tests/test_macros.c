#include "header.h"

int main() {
    int a = HEADER_VERSION;
    int b = HEADER_MACRO(5);
    const char* s = STRINGIFY(hello);
    const char* t = TOSTRING(123);
    int c = COMPLEX_MACRO(10, 20);
    return 0;
}
