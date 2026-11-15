# The GTK REPL of MetaC

A GTK3 front-end for the MetaC compiler.

## The idea

The terminal REPL (`repl/repl.c`) evaluates one line at a time.  That is
handy for poking at the compiler, but it is a poor way to write code: you
cannot keep a function definition around, the comments scroll away and
there is only ever one anonymous buffer.

This program is an editor-like front-end for the very same compiler core.
The central idea is:

> **the tabs are the files of a `metac_filesystem_t`**

MetaC already abstracts the file system (`os/metac_file.h`).  A compiler
session is handed such a file system through the `ui_interface`'s
`GetFileSystem` callback, and that is exactly what this program does:

```
+------------------------------------------------+
|  GtkNotebook                                  |
|    tab "hello.mtc"   <-- a file in the FS     |
|    tab "utils.mtc"   <-- a file in the FS     |
+------------------------------------------------+
         |                    ^
         |  metac_filesystem_t provided by the GTK app
         v                    |
    the compiler sees exactly these files
    (#include "utils.mtc" resolves to another tab)
```

There is **one** compiler session for the whole application, so all the
files share the same globals, the same scope and the same runtime heap.
That is what makes `#include` useful: a file can use the declarations of
another file.

## What you get

* one tab per file of the virtual file system,
* a file browser (left hand side) showing the contents of the file system,
* a source editor with MetaC syntax highlighting, line numbers and a
  "toggle comment" command (`Ctrl+/`) -- the code you write stays exactly
  as you typed it, comments included,
* a transcript that echoes the code -- comments and all -- and shows the
  result of every statement right below it,
* the whole compiler: types are values, `.Compiler` is available and
  declarations really end up in the session scope.

## Building

```sh
cd repl/gtk
./build_gtk3.sh
./gtk_repl
```

You need a GTK3 development package (`libgtk-3-dev`) and `pkg-config`.
The build script contains a recipe for unpacking the development files
into a local prefix when you cannot install packages (no root access).

A quick self test, which runs a small program through the whole pipeline
and prints the transcript:

```sh
./gtk_repl --selftest
```

## Using it

| Key / button        | what it does                                        |
|---------------------|-----------------------------------------------------|
| `Ctrl+Return`       | evaluate the current file                            |
| `Ctrl+/`            | comment / uncomment the selected lines               |
| `Ctrl+N`            | new file                                            |
| `Run`               | evaluate the current file (same as `Ctrl+Return`)    |
| `Globals`           | dump the globals of the session                      |
| `Heap`              | dump the runtime heap of the session                 |
| `New file`          | create a file in the file system (and a tab for it)  |
| `Rename` / `Delete` | rename / remove a file of the file system            |

The **Mode** combo selects what happens to a statement of the file:

| Mode           | REPL command | what it does                                  |
|----------------|--------------|-----------------------------------------------|
| Evaluate       | `:ee`        | evaluate the expression, print the value      |
| Type           | `:es`        | analyse the expression, print its type       |
| Declare        | `:ds`        | put the declaration into the session scope   |
| Parse          | `:d`         | only parse it, print the parse tree          |

### The transcript

```
=== run #1 ===

   the comment of the declaration
  decl  int x = 5;
                                    -> int x = 5;

   the comment of the expression
  eval  sizeof(x)                                      -> 4
```

Comments are parsed into `stmt_comment` nodes, so they are echoed back
verbatim and the result of every statement appears right below the
comment that introduces it.

## How it works

MetaC's semantic analyser is written for a fiber based scheduler: it can
suspend itself (`YIELD`) while it waits for a type to resolve.  The
project solves this with `os/metac_task.c`: `RunWorkerThread()` runs a
cooperative scheduler and lets the "special" fiber (the REPL) suspend and
resume.

This program reuses that machinery verbatim:

```
   GTK thread                      worker thread
   -----------                     -------------
   user presses Ctrl+Return  -->   the fiber wakes up
                                   parses + analyses + evaluates
   appends transcript        <--   pushes text into an async queue
```

The source buffer is parsed as a *sequence of statements*.  MetaC's
statement parser accepts both declarations and expressions (see
`MetaCParser_ParseStmt` in `parser/metac_parser.c`), so a scratchpad may
freely mix

```c
// a comment
int x = 5;
x + 1
```

### The file system

The GTK application implements the `metac_filesystem_functions_t` vtable
of `os/metac_file.h`:

| function                              | what the GTK app does                          |
|---------------------------------------|------------------------------------------------|
| `Init`                                | returns a context that points at the application |
| `Open`                                | looks the file up in the file list of the app   |
| `ReadEntireFileAndZeroTerminate`      | copies the content of the editor into a buffer  |
| `Close`                               | frees the handle                               |
| `GetFileInfo` / `GetFileSize`         | the size of the file                           |
| `ReadUntilBufferFullAndZeroTerminate` | partial read                                   |
| `GetFilesystemSize`                   | the sum of the sizes of all the files          |

The content of a file is the content of its editor buffer, so the
compiler always sees what you see.  The editor and the compiler run in
different threads, therefore the content is copied under a mutex.

## Known limitations of the compiler

These are limitations of the compiler core, not of the front-end; the
front-end reports them instead of crashing:

* **reading a global variable** in an evaluated expression is not
  implemented in the code generator
  (`MetaCCodegen_AccessVariable` in `codegen/metac_codegen.c` hits an
  `assert(!"Not implemented")`).  The front-end detects the situation and
  prints a proper error message.
* the type arithmetic of the README (`T = int+1, T`) is not implemented
  in the code generator either.

## A note on the coroutine backend

The build uses `-DMETAC_CORO_FCONTEXT`, which selects the boost.context
based implementation of the coroutine layer (`os/metac_coro_fcontext.c`)
instead of the default libaco one (`3rd_party/libaco`).

The reason is a stack alignment problem in libaco: the stacks of the
coroutines are not 16 byte aligned, so every call into a function that
uses SSE instructions without an aligned stack (`printf()` for example)
crashes with `SIGSEGV` on a current glibc.  The boost.context backend
keeps the alignment and therefore works.
