const GC = @This();

const std = @import("std");

const Objects = @import("../types/object.zig");
const Environment = @import("../environment/environment.zig");
const Object = Objects.Object;
const EvaluationError = @import("../evaluator/evaluator.zig").EvaluatorError;

alloc: std.mem.Allocator,
memory: std.SinglyLinkedList(*TrackedObject),

const TrackedObject = struct {
    mark: bool,
    obj: *Object,
};

pub fn init(alloc: std.mem.Allocator) GC {
    return .{
        .alloc = alloc,
        .memory = std.SinglyLinkedList(*TrackedObject),
    };
}

pub fn add(self: *GC, obj: *Object) EvaluationError!void {
    const tracked = TrackedObject{
        .mark = false,
        .obj = obj,
    };
    const trackedPtr = self.alloc.create(TrackedObject) catch return EvaluationError.MemoryAllocation;
    trackedPtr.* = tracked;
    self.memory.prepend(trackedPtr) catch return EvaluationError.MemoryAllocation;
}

pub fn getTrackedObject(self: *GC, obj: *Object) ?*TrackedObject {
    var head = self.memory.first;
    while (head) |item| {
        const headObj = head.?.data.*.obj;
        if (Objects.compareObjects(obj.*, headObj.*)) {
            return head.?.data;
        }
        head = item.*.next;
    }
    return null;
}

pub fn markObject(self: *GC, obj: *Object) void {
    const tracked = self.getTrackedObject(obj);
    if (tracked == null) return;
    tracked.?.mark = true;
    switch (obj.*) {
        .array => |array| {
            for (array.elements.items) |arrObj| {
                self.markObject(arrObj);
            }
        },
        else => {},
    }
}

pub fn markEnvironment(self: *GC, env: *Environment) void {
    const iter = env.store.iterator();
    while (iter.next()) |entry| {
        const entryPtr = entry.value_ptr.*;
        self.markObject(entryPtr);
    }
    if (env.outer) |outer| {
        self.markEnvironment(outer);
    }
}
