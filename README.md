# meta-c
MetaC or meta-c (for domain name reasons)

Is a language that aims to be compatible with C99

You can use a project written in C99 and you should be able to compile it with the metac compiler.

It improves on C by providing more extensive meta programming capabilites than C.

![Build Status](https://github.com/UplinkCoder/metac/actions/workflows/ci.yml/badge.svg)

## Note

This project is not currently finished and some items below described below might not be functional.

# Startup
Upon startup metac will try to read a file named `metac_compiler_interface.h` and search for struct definitions in there.
Currently it's expected that only one struct is defined.
It will then put an instance of that struct into the `.` namespace/scope which is accessible as `.compiler`
Right now the `.compiler` does not get populated with current compiler-state information, but this is supposed to change.

# Compiler-API

Within the langauge you can access a struct of type metac_compiler_t by using the special name `.compiler`.
Which exposes the functions that you can call.

During compilation you can have functions run and those functions can access the compiler via the API.
Which allows you to use powerful meta-programming techniques

# Type Expressions (works partially)

In MetaC types are expressions.
So in `EE`-mode (Expression Evaluation) you can type `int` and the return value will be `int`
Or you could write `double` and the type-value `double` will be returned.
At the time of writing MetaC doesn't do type checking on operations so you can enter expressions such as
`int - 1` which will result in an integer value (268435467).
That is because at the time of writing types are represented as a 32bit wide bitfield.
The type kind in the leftmost (most-significant) bits and an index in the rightmost (least-significant) bits.
This is also the reason why `sizeof(typeof(void))` is 4; since the runtime representation of a type is 4 bytes big.
We can force an integer to be interpreted as a type by assigning it to a variable of type `type`.
```C
:ds
type T;
:ee
T=int+1,T
```
if you execute the code above in the repl the result should be `long` 
similarly if you execute
```C
:ds
type T;
:ee
T=int-1,T
```
The result should be `short`.

At the moment this may seem like a quirky toy feature but, it will be one of the corner-stones of metac programming in MetaC.


# Yield Expressions (doesn't work yet)

`yield` is a keyword in metac that has the effect of pausing the execution of the function you are currently in and returning a value.
When you then call that same function again it will not start at the beginning but rather it will continue after the point where you yielded.

All local variables are preserved.
So is any `global` state which is annotated with `@TaskLocal`.

# Building the repl

First the auto-generated code has to be generated.
Just execute `./gen_code.sh` in the directory you cloned the repository into.
Then to build the repl simply `cd repl` and compile the file `linenoise_repl.c`
An example commandline would look like `gcc -Os linenoise_repl.c -o repl -lpthread -lm`
I have only tested this on `x86_64` and `aarch64` with `ubuntu 16.04` and `ubuntu 18.04` so anything else might run into unforseen trouble.

If it does fail to compile please open an issue

- NOTE: when you supply the defines `PRINT_BYTECODE=1` and `PRINT_CODE=1` the repl will print the first 24 instructions when evaluating expressions.

#Forign language blocks (not currently impemented)

forign langauge blocks are a tool to integrate other languauges with meta-c
they are essentailly heredoc strings with a language tag that allows source processors to pick out the block it is interested in.
an FLB starts with `\n@FLB{\n` for an untagged block or with`\n@(FLB, "some-tag-string"){\n` for a tagged forign language block
and it ends with `\n}FLB\n`

Even though the { syntax does suggest nesting;
Forign langauge blocks do not nest currently.

This may be subject to change in the future.

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
EE> sizeof(x2)
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

