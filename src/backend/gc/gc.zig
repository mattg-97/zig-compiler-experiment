const GC = @This();

const std = @import("std");

const Objects = @import("../types/object.zig");
const Environment = @import("../environment/environment.zig");
const Object = Objects.Object;
const EvaluationError = @import("../evaluator/evaluator.zig").EvaluatorError;
const debugMemory = @import("../../utils/memory.zig").debugMemory;

alloc: std.mem.Allocator,
head: *Object,
count: u64,

pub fn init(alloc: std.mem.Allocator) GC {
    return .{
        .alloc = alloc,
        .head = Objects.String.new(alloc, "head") catch @panic("unable to create head node for GC"),
        .count = 0,
    };
}

pub fn add(self: *GC, obj: *Object) void {
    obj.next = self.head.next;
    self.head.next = obj;
}

pub fn markObject(self: *GC, obj: *Object) void {
    if (obj.mark) return;
    obj.*.mark = true;
    switch (obj.*.data) {
        .array => |array| {
            for (array.elements.items) |arrObj| {
                self.markObject(arrObj);
            }
        },
        else => {},
    }
}

pub fn markEnvironment(self: *GC, env: *Environment) void {
    var iter = env.store.iterator();
    while (iter.next()) |entry| {
        const entryPtr = entry.value_ptr.*;
        self.markObject(entryPtr);
    }
    if (env.outer) |outer| {
        self.markEnvironment(outer);
    }
}

pub fn sweep(self: *GC) void {
    var deallocCount: u64 = 0;
    var node = self.head;
    while (node.next) |nextNode| {
        if (nextNode.mark) {
            node.next = nextNode.next;
            deallocCount += debugMemory(nextNode);
            self.alloc.destroy(nextNode);
        } else {
            node = nextNode;
        }
    }
    node = self.head;
    while (node.next) |nextNode| {
        nextNode.*.mark = false;
        node = nextNode;
    }
    std.log.info("Garbage collector deallocated: {d} Bytes\n", .{deallocCount});
}
