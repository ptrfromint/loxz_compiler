const std = @import("std");
const Alignment = std.mem.Alignment;
const build_options = @import("build_options");

const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const Value = @import("value.zig").Value;
const Obj = Value.Obj;
const VirtualMachine = @import("virtual_machine.zig").VirtualMachine;

/// A Mark-Sweep Garbage Collector that implements the std.mem.Allocator interface.
/// This allows it to be used as a drop-in replacement for standard allocators
/// throughout the codebase.
pub const GarbageCollector = struct {
    /// The underlying allocator used for actual memory management.
    parent_allocator: std.mem.Allocator,

    /// Total bytes currently allocated by the user program.
    bytes_allocated: usize,

    /// The threshold at which the next garbage collection will be triggered.
    next_gc: usize,

    /// References to the VM and Compiler to locate roots (stack, globals, etc.).
    /// Ideally one of these is always active if we are allocating objects.
    vm: ?*VirtualMachine,
    compiler: ?*Compiler,

    /// The stack of objects that have been marked but whose children haven't been visited yet.
    /// Used during the "Trace References" phase.
    gray_stack: std.ArrayList(*Obj),

    pub fn init(parent_allocator: std.mem.Allocator) GarbageCollector {
        return .{
            .parent_allocator = parent_allocator,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024, // Start with 1MB threshold
            .vm = null,
            .compiler = null,
            .gray_stack = .empty,
        };
    }

    pub fn deinit(self: *GarbageCollector) void {
        // NOTE: We don't free the user objects here; we assume the VM/Compiler
        // cleanup lifecycle handles explicit destruction or they are just leaked
        // if the process is ending.
        self.gray_stack.deinit(self.parent_allocator);
    }

    /// Returns the Allocator interface to be used by the rest of the application.
    pub fn allocator(self: *GarbageCollector) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
        const self: *GarbageCollector = @ptrCast(@alignCast(ctx));

        // Check if we need to collect garbage BEFORE allocating.
        if (self.bytes_allocated > self.next_gc) {
            self.collectGarbage() catch |err| {
                // We proceed even if GC fails, hoping we can still allocate.
                std.debug.print("GC Error during allocation: {}\n", .{err});
            };
        }

        // Perform the allocation using the parent allocator.
        const result = self.parent_allocator.rawAlloc(len, alignment, ret_addr);

        // Update counters if succeeded
        if (result != null) {
            self.bytes_allocated += len;
        }

        return result;
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *GarbageCollector = @ptrCast(@alignCast(ctx));

        // If growing, check if we need to do a cleanup.
        if (new_len > memory.len) {
            if (self.bytes_allocated + (new_len - memory.len) > self.next_gc) {
                self.collectGarbage() catch |err| {
                    // We proceed even if GC fails, hoping we can still allocate.
                    std.debug.print("GC Error during allocation: {}\n", .{err});
                };
            }
        }

        if (self.parent_allocator.rawResize(memory, alignment, new_len, ret_addr)) {
            if (new_len > memory.len) {
                self.bytes_allocated += new_len - memory.len;
            } else {
                self.bytes_allocated -= memory.len - new_len;
            }
            return true;
        }

        return false;
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *GarbageCollector = @ptrCast(@alignCast(ctx));

        // If growing, check if we need to do a cleanup.
        if (new_len > memory.len) {
            if (self.bytes_allocated + (new_len - memory.len) > self.next_gc) {
                self.collectGarbage() catch |err| {
                    // We proceed even if GC fails, hoping we can still allocate.
                    std.debug.print("GC Error during allocation: {}\n", .{err});
                };
            }
        }

        const result = self.parent_allocator.rawRemap(memory, alignment, new_len, ret_addr);

        if (result != null) {
            if (new_len > memory.len) {
                self.bytes_allocated += new_len - memory.len;
            } else {
                self.bytes_allocated -= memory.len - new_len;
            }
        }

        return result;
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void {
        const self: *GarbageCollector = @ptrCast(@alignCast(ctx));

        self.parent_allocator.rawFree(memory, alignment, ret_addr);

        self.bytes_allocated -= memory.len;
    }

    /// The main entry point for the Garbage Collector.
    /// Runs the Mark-Sweep algorithm.
    pub fn collectGarbage(self: *GarbageCollector) !void {
        if (build_options.show_debug) std.debug.print("-- gc begin (allocated: {d})\n", .{self.bytes_allocated});

        try self.markRoots();
        try self.traceReferences();
        self.sweep();

        // Adjust the next threshold.
        // A simple heuristic: GC again when we've grown by a factor of 2.
        self.next_gc = self.bytes_allocated * 2;

        if (build_options.show_debug) std.debug.print("-- gc end (allocated: {d}, next: {d})\n", .{ self.bytes_allocated, self.next_gc });
    }

    /// Identify all objects directly reachable from the VM (stack, globals)
    /// and the Compiler (if active).
    fn markRoots(self: *GarbageCollector) !void {
        if (self.vm) |vm| {
            try vm.markRoots(self);
        }
        if (self.compiler) |compiler| {
            try compiler.markRoots(self);
        }
    }

    /// Traverse the object graph starting from the gray stack (roots).
    fn traceReferences(self: *GarbageCollector) !void {
        while (self.gray_stack.items.len > 0) {
            const obj = self.gray_stack.pop();
            try self.blackenObject(obj.?);
        }
    }

    /// Marks all objects referenced by `obj`.
    fn blackenObject(self: *GarbageCollector, obj: *Obj) !void {
        if (build_options.show_debug) std.debug.print("{s}Marking {*}\n", .{ debug.Color.Dim, obj });

        switch (obj.kind) {
            .closure => |*closure| {
                try self.markObject(closure.function);
                for (closure.upvalues.items) |upvalue| {
                    try self.markObject(upvalue);
                }
            },
            .function => |*func| {
                if (func.name) |name| try self.markObject(name);
                for (func.chunk.constants.items) |val| {
                    try self.markValue(val);
                }
            },
            .upvalue => |*upvalue| {
                try self.markValue(upvalue.closed);
            },
            // Native functions and strings have no outgoing references to other Objects.
            .native_function, .string => {},
        }
    }

    /// Helper to mark a generic Value.
    pub fn markValue(self: *GarbageCollector, value: Value) !void {
        if (value == .obj) try self.markObject(value.obj);
    }

    /// Helper to mark a generic Object.
    /// If the object is not marked, mark it and add to gray stack.
    pub fn markObject(self: *GarbageCollector, obj: ?*Obj) !void {
        if (obj) |o| {
            if (o.is_marked) return;

            o.is_marked = true;
            try self.gray_stack.append(self.parent_allocator, o);
        }
    }

    /// Iterate through the linked list of all objects.
    /// Free any object that is NOT marked.
    /// Unmark everything else for the next cycle.
    fn sweep(self: *GarbageCollector) void {
        var previous: ?*Obj = null;

        // Find the head of the object list.
        // It resides in the VM or Compiler.
        var head_ptr: *?*Obj = undefined;
        if (self.vm) |vm| {
            head_ptr = &vm.objects;
        } else if (self.compiler) |comp| {
            head_ptr = comp.objects;
        } else {
            // If we have no active owner of the object list, we can't sweep safely
            // (or everything is garbage, but we lost the pointer).
            return;
        }

        var object = head_ptr.*;

        while (object) |obj| {
            if (obj.is_marked) {
                // Object is reached. Unmark for next time.
                obj.is_marked = false;
                previous = obj;
                object = obj.next;
            } else {
                // Object is unreached (Garbage).
                const unreached = obj;
                object = obj.next;

                // Unlink
                if (previous) |prev| {
                    prev.next = object;
                } else {
                    head_ptr.* = object;
                }

                // Free the memory.
                // This calls self.free(), which updates bytes_allocated.
                unreached.free(self.allocator());
            }
        }
    }
};
