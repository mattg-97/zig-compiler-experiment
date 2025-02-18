const Value = @This();

const std = @import("std");

const AST = @import("../../frontend/parser/ast.zig");
const Envrionment = @import("../environment/environment.zig");

pub const ValueType = enum {
    VAL_BOOL,
    VAL_INT,
    VAL_FLOATING,
    VAL_STRING,
    VAL_NIL,
    VAL_RETURN,
    VAL_FUNCTION,
    ERROR,
    pub fn toString(self: ValueType) []const u8 {
        switch (self) {
            .VAL_BOOL => return "BOOLEAN",
            .VAL_INT => return "INTEGER",
            .VAL_FLOATING => return "FLOAT",
            .VAL_STRING => return "STRING",
            .VAL_NIL => return "NULL",
            .VAL_RETURN => return "RETURN",
            .VAL_FUNCTION => return "FUNCTION",
            .ERROR => return "ERROR",
        }
    }
};

pub const Error = struct {
    message: []const u8,
};

pub const Function = struct {
    params: []*AST.Identifier,
    body: AST.BlockStatement,
    env: *Envrionment,
};

pub const As = union(enum) {
    boolean: bool,
    integer: i64,
    string: []const u8,
    nil: ?void,
    ret: *Value,
    err: Error,
    func: Function,
};

valType: ValueType,
as: As,

pub inline fn isFunc(value: Value) bool {
    return value.valType == ValueType.VAL_FUNCTION;
}

pub inline fn asFunc(value: Value) Function {
    return value.as.func;
}

pub inline fn funcValue(b: Function) Value {
    return Value{ .valType = ValueType.VAL_FUNCTION, .as = .{ .func = b } };
}

pub inline fn isBool(value: Value) bool {
    return value.valType == ValueType.VAL_BOOL;
}

pub inline fn asBool(value: Value) bool {
    return value.as.boolean;
}

pub inline fn boolValue(b: bool) Value {
    return Value{ .valType = ValueType.VAL_BOOL, .as = .{ .boolean = b } };
}

pub inline fn isInt(value: Value) bool {
    return value.valType == ValueType.VAL_INT;
}

pub inline fn asInt(value: Value) i64 {
    return value.as.integer;
}

pub inline fn intValue(i: i64) Value {
    return Value{ .valType = ValueType.VAL_INT, .as = .{ .integer = i } };
}

pub inline fn isString(value: Value) bool {
    return value.valType == ValueType.VAL_STRING;
}

pub inline fn asString(value: Value) []const u8 {
    return value.as.string;
}

pub inline fn stringValue(s: []const u8) Value {
    return Value{ .valType = ValueType.VAL_STRING, .as = .{ .string = s } };
}

pub inline fn isNil(value: Value) bool {
    return value.valType == ValueType.VAL_NIL;
}

pub inline fn asNil(value: Value) ?void {
    return value.as.nil;
}

pub inline fn nilValue() Value {
    return Value{ .valType = ValueType.VAL_NIL, .as = .{ .nil = null } };
}
pub inline fn isReturn(value: Value) bool {
    return value.valType == ValueType.VAL_RETURN;
}

pub inline fn asReturn(value: Value) Value {
    return value.as.ret.*;
}

pub inline fn returnValue(retValue: Value) Value {
    return Value{ .valType = ValueType.VAL_RETURN, .as = .{ .ret = @constCast(&retValue) } };
}

pub inline fn isError(value: Value) bool {
    return value.valType == ValueType.ERROR;
}

pub inline fn asError(value: Value) Error {
    return value.as.err;
}

pub inline fn errorValue(value: Error) Value {
    return Value{ .valType = ValueType.ERROR, .as = .{ .err = value } };
}

pub inline fn valuesEqual(a: Value, b: Value) bool {
    if (a.valType != b.valType) return false;
    switch (a.valType) {
        ValueType.VAL_STRING => return std.mem.eql(u8, asString(a), asString(b)),
        ValueType.VAL_BOOL => return asBool(a) == asBool(b),
        ValueType.VAL_INT => return asInt(a) == asInt(b),
        else => return false,
    }
}

pub fn printValue(value: Value) void {
    switch (value.valType) {
        ValueType.VAL_BOOL => {
            const toPrint: []const u8 = if (asBool(value)) "true" else "false";
            std.debug.print("{s}", .{toPrint});
        },
        ValueType.VAL_INT => std.debug.print("{d}", .{asInt(value)}),
        ValueType.VAL_STRING => std.debug.print("{s}", .{asString(value)}),
        ValueType.VAL_NIL => std.debug.print("NULL", .{}),
        ValueType.VAL_FUNCTION => std.debug.print("FUNCTION", .{}),
        ValueType.VAL_RETURN => {
            const ret = value.asReturn();
            ret.printValue();
        },
        else => std.debug.print("Unable to print value", .{}),
    }
}
