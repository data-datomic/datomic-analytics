# Microtasks

It is strongly recommended that students who want to apply to the radare2 GSoC/RSoC projects will perform a small task, to get the idea of students’ capabilities and make them familiar with radare2's codebase, structure and development process. Note: some tasks mentioned here are "meta" issues, which are big lists of smaller tasks. Of course finishing such large issues is impossible in a short period of time, so this means the student can take a few list items from those bugs as their microtask. Here is the list of such “qualification” tasks:

### INDEX

## Disassemblers and assemblers

Implementing the support for any new architectire counts as a microtask. See [New-Architecture](https://github.com/radare/radare2/labels/New%20Architecture) label for pending issues.
Nevertherless we've chosen a few as the most important ones:

### LLVM bitcode [#2778](https://github.com/radare/radare2/issues/2778)

LLVM bitcode is a common format of a bytecode, used in many different compilers and tools.

See [llvm-bcanalyzer](https://github.com/llvm-mirror/llvm/blob/master/tools/llvm-bcanalyzer/llvm-bcanalyzer.cpp) and [BitCodeReader.cpp](https://github.com/llvm-mirror/llvm/blob/master/lib/Bitcode/Reader/BitcodeReader.cpp) on how to implement its parsing and decoding. See also [#3896](https://github.com/radare/radare2/issues/3896) for integration with Mach-O format parser.

### GPU architectures support [#12161](https://github.com/radare/radare2/issues/12161)
Modern GPUs are now basically a powerful embedded computer network. Every video card has plenty of different chips and firmwares, fused and loaded, running. Radare2 can provide a convenient interface to reverse engineer and audit these firmwares or loaded programs, as well as help open source video drivers efforts.

### Python bytecode [#4228](https://github.com/radare/radare2/issues/4228)

Currently there is support already for disassembling Python bytecode. But like with LUA the architecture is largely untested and can be easily broken. Moreover, the analysis plugin is not implemented, so a lot of information is still missing in the output. We need to add proper tests and see if there are bugs (and fix them).

See [universal python disassembler](https://github.com/evanw/unwind) and [python cross-version decompiler](https://github.com/rocky/python-uncompyle6) for example and [basic](https://github.com/radare/radare2-extras/tree/master/libr/bin/format/pyc) [implementation](https://github.com/radare/radare2-extras/blob/master/libr/asm/p/asm_pyc.c) in radare2-extras for current state of it.

![image](https://image.slidesharecdn.com/tailbytespygotham-140819135745-phpapp02/95/tco-in-python-via-bytecode-manipulation-7-638.jpg)

### Java
Java support landed in radare2 a long time ago. At the same time it is largely unused, full of bugs and poorly written. Some code (e.g. [anal_extra](https://github.com/radare/radare2/blob/master/libr/anal/anal_ex.c)) doesn't really fit and can be moved/refactored on top of the modern radare2 architecture design.

### Improving WebAssembly (WASM) support

- ESIL (emulation should be super easy since it is stack based too)
- Enrich the view of the data, like globals, calls, etc.. [#12225](https://github.com/radare/radare2/issues/12225)
- Proper decompilation via external plugin (not r2dec, maybe via radeco)

### Reviving .NET bytecode support
It is widely adopted and there are many tools available for decompilation. On the other hand radare2 provides many useful features across all architectures and scripting capabilities, which can help to improve the state of .NET reverse engineering tooling. Currently the most basic MSIL support lives in [radare2 extras](https://github.com/radare/radare2-extras/tree/master/libr/asm/arch/msil). It can be revived, improved and enhanced to add newer format of the .NET bytecode. See other tools that work with .NET bytecode:

- [ILSpy](https://github.com/icsharpcode/ILSpy)
- Telerik [JustDecompile engine](https://github.com/telerik/justdecompileengine)
- JetBrains [dotPeek](https://www.jetbrains.com/decompiler)

## Analysis

The current code analysis has many caveats and issues which need addressing. Fixing them and writing more tests is important to stabilize and enhance radare2's analysis engine.

[See these issues](https://github.com/radare/radare2/issues?q=is%3Aissue+is%3Aopen+label%3Aanal)

### Heap analysis [#5390](https://github.com/radare/radare2/issues/5390)
Currently radare2 has support for heap exploration and analysis, but the feature is still basic and can be improved. Additionally, other allocators can be added (jemalloc, etc.), but this should be done after a proper refactoring, because heap analysis shouldn't depend on the debugger backend, and we may be able to use different heap tools.

The most important part of supporting heap analysis is to create a new subset of commands, and put all heap analyis under data analysis or debugger-wide features, not in the target debugger backend. Many things are also done in C, when they could be solved with format strings.

### Basefind [#10725](https://github.com/radare/radare2/issues/10725)
There are plenty of external scripts and plugins for finding the most probable base for raw firmware images. Opening raw firmwares with radare2 is a common use case, so it makes sense to implement it as a part of radare2 core.

## Anal Classes for C++/ObjectiveC/Swift/Dlang/Java

Anal classes, accessible under the `ac` command, are a new feature of r2 which has only recently been merged into master.
They provide a way to both manually and automatically manage and use information about classes in the binary.

### Pulling class info from bin to anal [#12600](https://github.com/radare/radare2/issues/12600)

Bin classes, accessible using `ic`, are static info recovered from, for example, symbol names.
They are not editable and provide only a single source for class information.

There should be a command that takes this info and pulls it into anal classes, so the they can be manually edited, enhanced with more information, etc.

### Add size info to anal class vtables [#12601](https://github.com/radare/radare2/issues/12601)

Right now, vtables in `aCv` only have an address, but no size.
This should be added to the sdb record and also be represented in the size of the flag for the vtable.

### Devirtualize method calls using anal class vtables [#12603](https://github.com/radare/radare2/issues/12603)

Consider the following call: `call dword [eax + 0x6c]`
Let's assume eax is the base pointer of a vtable we have saved in anal classes and we want to find out the actual address of the called method.

So there should be a command that takes the offset (in this case 0x6c) and looks up the actual destination.
It should be possible to call this command with a specific class, so it only looks into its vtable, or without a class, so it gives a list of possible destinations for all vtables that are not too small for the offset (partially requires [#12601](https://github.com/radare/radare2/issues/12601))

When that is implemented, one could also add a command that does the same thing, but automatically takes the offset from the opcode at the current seek.

### Add anal classes to Vb [#12604](https://github.com/radare/radare2/issues/12604)

`Vb` already supports browsing bin classes. The same thing should be implemented for anal classes.

## META - Signatures [#6947](https://github.com/radare/radare2/issues/6947)

Radare2 has a good support for loading and creating signatures, but it is not yet complete, thus
improving the signature contents (their variables, arguments, types, local flags and comments),
their testing coverage and user interface (commands, [reviving `rasign2` tool](https://github.com/radare/radare2/issues/9336)).
Apart from that, [better integration with analysis loop](https://github.com/radare/radare2/issues/5331) is
required for the best results of autoanalysis.
Of course all these features are worthless without the actual signatures provided, thus the task to
[create the default pack](https://github.com/radare/radare2/issues/7310).

## META - Graphs [#6967](https://github.com/radare/radare2/issues/6967)

### Binary diffing - [radiff2](https://github.com/radare/radare2/labels/radiff2)

Bindiffing has been a known feature of radiff2, but it has been unmaintained for years. Improving the output, adding visual diffing modes, using analysis results and optimizing speed are the most important things here.

### Node groups

Being able to select multiple nodes in the graph and group them to colorize them and specify a name for them. [#2952](https://github.com/radare/radare2/issues/2952)

### Save/restore graph state

This task is necessary when node grouping or layout have changed, this information can be stored in projects by just reusing the `agn` and `age` commands to recreate a graph and feeding the body of the nodes in base64.

## META - RAGG2 [#6949](https://github.com/radare/radare2/issues/6949)

Ragg2 - simplistic [compiler for C-like syntax](http://radare.today/posts/payloads-in-c/) into tiny binaries for x86-32/64 and arm. Programs generated by ragg2 are relocatable and can be injected in a running process or on-disk binary file. Fixing  ragg2 issues will help a lot for creating small payloads for exploiting tasks.

## RAFIND2 - Various enhancement

Rafind2 - binwalk parity
Various issues to improve rafind2 such as being able to extract known file types automatically and recursively if the file is an archive (a la binwalk).

- [11163](https://github.com/radare/radare2/issues/11163) - `binwalk -Me` support
- [Other rafind2 issues](https://github.com/radare/radare2/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Arafind2+)

## Refactoring

### Sdbtization
Radare2 is being slowly refactored to store all the information about session, user metadata and state of debugger in the [SDB](https://github.com/radare/sdb) - simple key-value database. This work still ungoing. So helping us with a few sdbtization bugs will introduce you into the radare2 codebase structure.

We have decided to not sdbize everything, and use RBTree and RDict when necessary, so be sure to ask developers before starting. Also, some places in r2 (like the version bin parser) are using SDB in a poor way. Fixing those cases counts too.

[See issues](https://github.com/radare/radare2/issues?q=is%3Aopen+is%3Aissue+label%3Asdbtization)

### ESILization
Radare2 has its own intermediate language - ESIL, but not yet support it for all architectures. So
the task is to add ESIL support to any architecture, which doesn't has it yet.
[See issues](https://github.com/radare/radare2/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aopen%20label%3Aesil) for the related bugs.

* finish 8051 esil
* implement ARC esil emulation
* support packed registers (mmx and such)
* support floating point
* support MMX/SSE/AVX/other SIMD extensions

* Implement memory access callback apis in unicorn and make the unicorn plugin for r2 work

## File formats

Implementing the support for any new file format counts as a microtask. See [New File-Format](https://github.com/radare/radare2/labels/New%20File-Format) label for pending issues.
Nevertherless we chosen a few as the most important ones:

### META - Portable Executable format [#921](https://github.com/radare/radare2/issues/921)
There are lot of missing features in the current PE file parser as you can see in this META Issue.

![](https://image.slidesharecdn.com/44con2013workshop-exploringtheportableexecutableformat-130916113833-phpapp01/95/exploring-the-portable-executable-format-18-638.jpg)

this requires a refactor of rbin that hasn't happened yet, but also, we want to have a .NET parser (already commited but not used) for PE, and bring back the MSIL disassembler.

## Debugging
### Improving reversible debugging

Radare2 already [supports](https://radareorg.github.io/radare2book/debugger/revdebug.html) a basic "Record and Replay" feature, but the support is still basic and quite unstable. [See issue #8198](https://github.com/radare/radare2/issues/8198) for more information. See also [issue #8996](https://github.com/radare/radare2/issues/8996) for adding the reverse continue/step support via gdb:// (GDB remote) protocol.
See also [Debugger Data Model](https://doar-e.github.io/blog/2017/12/01/debugger-data-model) article about same feature in WinDbg.

### Better support for Activities and Permissions (list them, references, etc)

Take ideas from Androguard, and be able to follow execution flow paths to understand which permissions are used in a specific region of code, how to reach a specific activity, etc.

### Support to spawn iOS Apps, not just programs
See `debugserver -x springboard` and such to spawn apps from the backboard otherwise they get killed.

## Miscellanous

### Improving bindings
There are valabind generated bindings and we want them fixed, also merge r2pipe asyncronous and synchronous bindings. See [radare2-bindings](https://github.com/radare/radare2-bindings) repository for more information. It has also a different approach - parsing radare2 headers using Clang bindings and generating them without any intermediate files. There is support for Python, Go, Rust and Haskell. It should be improved and better tested - writing autotests will help a lot.

### Improving scripts for importing from IDA Pro IDB files
Currently radare2 can use the [ida2r2](https://github.com/radare/radare2ida) script to import information from the IDA Pro IDB files. It uses the [python-idb](https://github.com/williballenthin/python-idb) library for parsing IDB files without IDA Pro installed. Improving both will allow importing more information - types, variable and argument names, structures and enums, etc is the main goal of this task.

### Improving regression suite and testing
Currently radare2 uses a custom solution for running regression tests. It is required to solve [numerours issues](https://github.com/radare/radare2-regressions/issues), along with improving parallel execution and performance. The next interesting idea is to setup and [reuse Godbolt](https://github.com/radare/radare2-regressions/issues/1549) compilation engine for generating tests for different compilers and compilation options. There is even a command line tool for interacting with Godbolt - [cce](https://github.com/ethanhs/cce).

