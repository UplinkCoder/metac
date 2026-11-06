#include "header.h"

#define CONCAT(a, b) a##b
#define CONCAT_WRAP(a, b) CONCAT(a, b)

int main() {
    int var_a = 10;
    int CONCAT_WRAP(var, _b) = 20;
    
    #if defined(HEADER_VERSION) && (HEADER_VERSION == 1)
        int check = 1;
    #endif

    int result = COMPLEX_MACRO(HEADER_MACRO(1), HEADER_MACRO(2));
    return result;
}
