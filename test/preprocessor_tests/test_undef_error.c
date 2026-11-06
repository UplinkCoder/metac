#define TEMP_VAL 42
#undef TEMP_VAL

int main() {
    // This should probably cause a compilation error if I uncommented it
    // int x = TEMP_VAL; 
    
    // Testing #error (this will stop compilation, so I won't use it in a way that breaks the script)
    // #error "This is an error"
    
    return 0;
}
