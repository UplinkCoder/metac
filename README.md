# meta-c
MetaC or meta-c (for domain name reasons)

Is a language that aims to be compatible with C99

You can use a project written in C99 and you should be able to compile it with the metac compiler.

It improves on C by providing more extensive meta programming capabilites than C.

## Note

This project is not currently finished and some items below descriped below might not be functional.

During compilation you can have functions run and those functions can access the compiler via an API. (The same API is also exposed when using MetaC as a library, rather than a standalone compiler)

# Startup 
Upon startup metac will try to read a file named `metac_compiler_interface.h` and search for struct definitions in there.
Currently it's expected that only one struct is defined.
It will then put an instance of that struct into the `.` namespace/scope which is accessible as `.compiler`
Right now the `.compiler` does not get populated with current compiler-state information, but this is supposed to change.

# Compiler-API

Within the langauge you access a struct of type metac_compiler_t by using the special name `.compiler`.
Which exposes functions that you can call.

# Yield Expressions

`yield` is a keyword in metac that has the effect of pausing the execution of the function you are currently in and returning a value.
When you then call that same function again it will not start at the beginning but rather it will continue after the point where you yielded.

All local variables are preserved.
So is any `global` state which is annotated with `@TaskLocal`.
