# meta-c
MetaC or meta-c (for domain name reasons)

Is a language that aims to be compatible with C99

You can use a project written in C99 and you should be able to compile it with the metac compiler.

It improves on C by providing more extensive meta programming capabilites than C.

## Note

This project is not currently finished and some items below descriped below might not be functional.

# Startup
Upon startup metac will try to read a file named `metac_compiler_interface.h` and search for struct definitions in there.
Currently it's expected that only one struct is defined.
It will then put an instance of that struct into the `.` namespace/scope which is accessible as `.compiler`
Right now the `.compiler` does not get populated with current compiler-state information, but this is supposed to change.

# Compiler-API

Within the langauge you access a struct of type metac_compiler_t by using the special name `.compiler`.
Which exposes functions that you can call.

During compilation you can have functions run and those functions can access the compiler via the API.
Which allows you to use powerful meta-programming techniques

# Yield Expressions

`yield` is a keyword in metac that has the effect of pausing the execution of the function you are currently in and returning a value.
When you then call that same function again it will not start at the beginning but rather it will continue after the point where you yielded.

All local variables are preserved.
So is any `global` state which is annotated with `@TaskLocal`.

# Building the repl

To build the repl simply `cd repl` and compile the file `linenoise_repl.c`
I have only tested this on `x86_64` and `aarch64` with `ubuntu 16.04` and `ubuntu 18.04` so anything else might run into unforseen trouble.

If it does fail to compile please open an issue

## Repl modes

The repl is mostly a development and debugging tool for me
and it serves to mitigate the current shortcommings of the unfinished parser.
Therefore it has modes which change the parser behavior.
By default it starts in `:ee` the `Expression Evaluation` mode.
In this mode expressions given on the command line will be parsed, semantically analyzed and evaluated.
The _value_ resulting from this evaluation will be returned.

Then there is `:es` the Expression semantic mode.
In this mode the expression will be parsed and semantically analyzed.
The _type_ of the expression will be returned.

Another imporant mode is the `:ds` Declaration semantic mode.
In this mode a declaration/definition will be parsed and inserted into the global scope.
Note that previous definitions will shaddow the following ones.
meaning
```C
EE> :ds
DS> int x;
DS> :ee
EE> sizeof(x)
EE> :ds
DS> int x[16];
DS> int x2[16];
DS> :ee
EE> sizeof(x)
EE> sizeof(a)
```
will result in
the output
```
4
4
64
```
showing that x cannot be modified after defining it

This might be unusual for a repl but this repl is mainly
a tool to develop the semantic and codegen portion of the compiler
without having to finish the parser.

The other modes can be seen when typing `:h`
they are mostly for debugging/developing the parser

