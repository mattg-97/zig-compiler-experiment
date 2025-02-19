const std = @import("std");

const EvaluatorError = @import("./evaluator.zig").EvaluatorError;
const Objects = @import("../types/object.zig");
const Object = Objects.Object;

pub const BuiltinFn = enum {
    len,
    first,
    last,
    pub fn call(self: BuiltinFn, alloc: std.mem.Allocator, args: std.ArrayList(*Object)) EvaluatorError!*Object {
        return switch (self) {
            .len => try len(alloc, args),
            .first => try first(alloc, args),
            .last => try last(alloc, args),
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

fn expectArgslen(alloc: std.mem.Allocator, expect: usize, args: std.ArrayList(*Object)) !?*Object {
    if (args.items.len != expect) {
        return try Objects.Error.new(alloc, "wrong number of arguments. got = {d}, want = {d}\n", .{ args.items.len, expect });
    } else return null;
}
