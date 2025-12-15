# zlox

#### NOTE: This is a personal learning project and it's not meant to be used in production for anything.

## Requirements
- The project was built in Zig 0.15.2. Any other version than 0.15.* is not guaranteed to be able to successfully
compile the project, due to Zig's constant breakage between updates, as it is pre-1.0 as of 15.12.2025. 

## Features
- Single-Pass Compiler, which goes directly from scanning to bytecode
- Virtual Machine with a Garbage Collector, which implements the `std.mem.Allocator` interface. This allows
for seamless integration with all Zig code that uses allocation of any kind within the VM
- Working REPL (Read-Eval-Print Loop), which interprets statements line by line as the user enters them

## Differences from `clox` from "Crafting Interpreters"

- This project is an implementation of the `clox` Lox interpreter from the "Crafting Interpreters" book,
written in Zig.
- It covers almost all chapters, excluding the final chapter on optimizations and string interning.
It doesn't follow the conventions of the book 1 : 1 everywhere, opting for more type-safe implementations
where possible, while still staying true to the original version.
- For example, in many places where direct pointers to the compiler's stack are used, indices are
used instead as a replacement.
- Also, the size of the stack and other containers is not limited to constants like 256, instead we are using the full
`std.ArrayList` of Zig. This means that `_long` versions of a lot of instructions are implemented to accommodate the
fact that indices can go up to the max of `u24` in size, instead of `u8` like in the book.
