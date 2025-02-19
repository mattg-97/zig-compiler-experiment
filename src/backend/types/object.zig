const std = @import("std");

const AST = @import("../../frontend/parser/ast.zig");
const Envrionment = @import("../environment/environment.zig");

pub const Object = union(enum) {
    nullObject: Null,
    integer: Integer,
    boolean: Boolean,
    function: Function,
    returnObject: Return,
    err: Error,
    pub fn typeName(self: Object) []const u8 {
        switch (self) {
            inline else => |case| case.toString(),
        }
    }
    pub fn toString(self: *Object) void {
        switch (self.*) {
            inline else => |case| case.print(),
        }
    }
};

pub const Null = struct {
    pub fn typeName(_: Null) []const u8 {
        return "null";
    }
    pub fn toString(self: Null) void {
        std.debug.print("{s}\n]", .{self.typeName()});
    }
};

pub const Integer = struct {
    value: i64,

    const Self = @This();

    pub fn new(value: i64) Self {
        return .{
            .value = value,
        };
    }
    pub fn typeName(_: Self) []const u8 {
        return "integer";
    }
    pub fn toString(self: Self) void {
        std.debug.print("{d}\n]", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    const Self = @This();

    pub fn new(value: bool) Self {
        return .{
            .value = value,
        };
    }
    pub fn typeName(_: Self) []const u8 {
        return "boolean";
    }
    pub fn toString(self: Self) void {
        const toPrint = if (self.value) "true" else "false";
        std.debug.print("{s}\n]", .{toPrint});
    }
};

pub const Error = struct {
    message: []const u8,

    const Self = @This();

    pub fn new(message: []const u8) Self {
        return .{
            .message = message,
        };
    }
    pub fn typeName(_: Self) []const u8 {
        return "error";
    }
    pub fn toString(self: Self) void {
        std.debug.print("ERROR: {s}\n]", .{self.message});
    }
};

pub const Return = struct {
    value: *Object,

    const Self = @This();

    pub fn new(obj: *Object) Self {
        return .{
            .value = obj,
        };
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

    pub fn new(params: std.ArrayList(AST.Identifier), body: *AST.BlockStatement, env: *Envrionment) Self {
        return .{
            .params = params,
            .body = body,
            .env = env,
        };
    }
    pub fn typeName(_: Self) []const u8 {
        return "function";
    }
    pub fn toString(_: Self) void {
        // TODO add function printing
        std.debug.print("FUNCTION: ", .{});
    }
};
