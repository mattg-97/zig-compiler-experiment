const std = @import("std");

const EvaluatorError = @import("./evaluator.zig").EvaluatorError;
const Objects = @import("../types/object.zig");
const Object = Objects.Object;

pub const BuiltinFn = enum {
    len,
    first,
    last,
    tail,
    push,
    puts,
    pub fn call(self: BuiltinFn, alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
        return switch (self) {
            .len => try len(alloc, args),
            .first => try first(alloc, args),
            .last => try last(alloc, args),
            .tail => try tail(alloc, args),
            .push => try push(alloc, args),
            .puts => try puts(alloc, args),
        };
    }
};

fn len(alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
    if (try expectArgslen(alloc, 1, args)) |err| {
        return err;
    }
    switch (args.items[0].*) {
        .string => |argString| {
            return Objects.Integer.new(alloc, @intCast(argString.value.len));
        },
        .array => |array| {
            return Objects.Integer.new(alloc, @intCast(array.elements.items.len));
        },
        else => {
            return Objects.Error.new(alloc, "len function not supported for this argument: {s}\n", .{args.items[0].typeName()});
        },
    }
}

fn first(alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
    if (try expectArgslen(alloc, 1, args)) |err| {
        return err;
    }
    switch (args.items[0].*) {
        .array => |array| {
            if (array.elements.items.len == 0) {
                return Objects.Null.new(alloc);
            }
            return array.elements.items[0];
        },
        else => return Objects.Error.new(alloc, "indexing not supported for this argument: {s}\n", .{args.items[0].typeName()}),
    }
}

fn last(alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
    if (try expectArgslen(alloc, 1, args)) |err| {
        return err;
    }
    switch (args.items[0].*) {
        .array => |array| {
            if (array.elements.items.len == 0) {
                return Objects.Null.new(alloc);
            }
            return array.elements.items[args.items.len];
        },
        else => return Objects.Error.new(alloc, "indexing not supported for this argument: {s}\n", .{args.items[0].typeName()}),
    }
}

fn tail(alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
    if (try expectArgslen(alloc, 1, args)) |err| {
        return err;
    }
    switch (args.items[0].*) {
        .array => |array| {
            const length = array.elements.items.len;
            var tailArray = std.ArrayList(*Object).init(alloc);
            if (length > 0) {
                var idx: usize = 1;
                while (idx < length) : (idx += 1) {
                    tailArray.append(array.elements.items[idx]) catch return EvaluatorError.MemoryAllocation;
                }
            }
            return Objects.Array.new(alloc, tailArray);
        },
        else => return Objects.Error.new(alloc, "tail function not supported for this argument: {s}\n", .{args.items[0].typeName()}),
    }
}

fn push(alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
    if (try expectArgslen(alloc, 2, args)) |err| {
        return err;
    }
    switch (args.items[0].*) {
        .array => |array| {
            var pushedArray = array.elements.clone() catch return EvaluatorError.MemoryAllocation;
            pushedArray.append(args.items[1]) catch return EvaluatorError.MemoryAllocation;
            return Objects.Array.new(alloc, pushedArray);
        },
        else => return Objects.Error.new(alloc, "tail function not supported for this argument: {s}\n", .{args.items[0].typeName()}),
    }
}

fn puts(alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
    if (args.items.len == 0) return EvaluatorError.MemoryAllocation;
    for (args.items) |item| {
        item.toString();
    }
    return Objects.Null.new(alloc);
}

fn expectArgslen(alloc: std.mem.Allocator, expect: usize, args: std.ArrayList(*Object)) !?*Object {
    if (args.items.len != expect) {
        return try Objects.Error.new(alloc, "wrong number of arguments. got = {d}, want = {d}\n", .{ args.items.len, expect });
    } else return null;
}
