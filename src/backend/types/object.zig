const std = @import("std");

const AST = @import("../../frontend/parser/ast.zig");
const Envrionment = @import("../environment/environment.zig");
const EvaluatorError = @import("../evaluator/evaluator.zig").EvaluatorError;
const BuiltinFunction = @import("../evaluator/builtins.zig").BuiltinFn;

pub fn compareObjects(obj1: *Object, obj2: *Object) bool {
    switch (obj1.*) {
        .nullObject => {
            switch (obj2.*) {
                .nullObject => return true,
                else => return false,
            }
        },
        .integer => |int1| {
            switch (obj2.*) {
                .integer => |int2| return int1.value == int2.value,
                else => return false,
            }
        },
        .boolean => |boolean1| {
            switch (obj2.*) {
                .boolean => |boolean2| return boolean1.value == boolean2.value,
                else => return false,
            }
        },
        .function => return @intFromPtr(obj1) == @intFromPtr(obj2),
        else => @panic("Cant compare."),
    }
}

pub const Object = union(enum) {
    nullObject: Null,
    integer: Integer,
    boolean: Boolean,
    function: Function,
    returnObject: Return,
    err: Error,
    string: String,
    builtin: BuiltIn,
    array: Array,
    const Self = @This();
    pub fn typeName(self: Self) []const u8 {
        switch (self) {
            inline else => |case| return case.typeName(),
        }
    }
    pub fn toString(self: *Self) void {
        switch (self.*) {
            inline else => |case| case.toString(),
        }
    }
};

pub const Null = struct {
    pub fn new(alloc: std.mem.Allocator) EvaluatorError!*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .nullObject = .{},
        };
        return ptr;
    }
    pub fn typeName(_: Null) []const u8 {
        return "null";
    }
    pub fn toString(self: Null) void {
        std.debug.print("{s}\n", .{self.typeName()});
    }
};

pub const Integer = struct {
    value: i64,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, value: i64) EvaluatorError!*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .integer = .{
                .value = value,
            },
        };
        return ptr;
    }

    pub fn typeName(_: Self) []const u8 {
        return "integer";
    }
    pub fn toString(self: Self) void {
        std.debug.print("{d}\n", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, value: bool) !*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .boolean = .{
                .value = value,
            },
        };
        return ptr;
    }
    pub fn typeName(_: Self) []const u8 {
        return "boolean";
    }
    pub fn toString(self: Self) void {
        const toPrint = if (self.value) "true" else "false";
        std.debug.print("{s}\n", .{toPrint});
    }
};

pub const Error = struct {
    message: []const u8,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, comptime fmt: []const u8, args: anytype) EvaluatorError!*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        const errMsg = std.fmt.allocPrint(alloc, fmt, args) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .err = .{
                .message = errMsg,
            },
        };
        return ptr;
    }
    pub fn typeName(_: Self) []const u8 {
        return "error";
    }
    pub fn toString(self: Self) void {
        std.debug.print("ERROR: {s}\n", .{self.message});
    }
};

pub const Return = struct {
    value: *Object,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, retVal: *Object) !*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .returnObject = .{
                .value = retVal,
            },
        };
        return ptr;
    }
    pub fn typeName(_: Self) []const u8 {
        return "return";
    }
    pub fn toString(self: Self) void {
        std.debug.print("RETURN: ", .{});
        self.value.*.toString();
    }
};

pub const Function = struct {
    params: std.ArrayList(AST.Identifier),
    body: *AST.BlockStatement,
    env: *Envrionment,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, params: std.ArrayList(AST.Identifier), body: *AST.BlockStatement, env: *Envrionment) !*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .function = .{
                .params = params,
                .body = body,
                .env = env,
            },
        };
        return ptr;
    }
    pub fn typeName(_: Self) []const u8 {
        return "function";
    }
    pub fn toString(self: Self) void {
        // TODO add function printing
        std.debug.print("FUNCTION: \n", .{});
        std.debug.print("params: (\n", .{});
        for (self.params.items) |param| {
            param.print();
            std.debug.print("\n", .{});
        }
        std.debug.print(")\n", .{});
        std.debug.print("BODY: \n", .{});
        for (self.body.statements.items) |body| {
            body.print();
            std.debug.print("\n", .{});
        }
    }
};

pub const String = struct {
    value: []const u8,
    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, string: []const u8) !*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .string = .{
                .value = string,
            },
        };
        return ptr;
    }
    pub fn typeName(_: Self) []const u8 {
        return "string";
    }
    pub fn toString(self: Self) void {
        std.debug.print("{s}\n", .{self.value});
    }
};

pub const BuiltIn = struct {
    function: BuiltinFunction,

    const Self = @This();

    pub fn new(alloc: std.mem.Allocator, func: BuiltinFunction) EvaluatorError!*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .builtin = .{
                .function = func,
            },
        };
        return ptr;
    }

    pub fn typeName(_: Self) []const u8 {
        return "builtin";
    }
    pub fn toString(_: Self) void {
        // TODO print builtins
        std.debug.print("TODO \n", .{});
    }

    pub fn call(self: *const BuiltIn, alloc: std.mem.Allocator, args: std.ArrayList(*Object)) !*Object {
        return try self.function.call(alloc, args);
    }
};

pub const Array = struct {
    elements: std.ArrayList(*Object),
    const Self = @This();
    pub fn new(alloc: std.mem.Allocator, elems: std.ArrayList(*Object)) EvaluatorError!*Object {
        const ptr = alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
        ptr.* = .{
            .array = .{
                .elements = elems,
            },
        };
        return ptr;
    }
    pub fn typeName(_: Self) []const u8 {
        return "builtin";
    }
    pub fn toString(_: Self) void {
        // TODO print builtins
        std.debug.print("TODO \n", .{});
    }
};
