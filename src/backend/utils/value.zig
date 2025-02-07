const Value = @This();

const std = @import("std");

const ValueType = enum {
    VAL_BOOL,
    VAL_INT,
    VAL_FLOATING,
    VAL_STRING,
    VAL_NIL,
};

pub const As = union {
    boolean: bool,
    integer: i64,
    string: []const u8,
};

valType: ValueType,
as: As,

pub fn isBool(value: Value) bool {
    return value.valType == ValueType.VAL_BOOL;
}

pub fn asBool(value: Value) bool {
    return value.as.boolean;
}

pub fn isInt(value: Value) bool {
    return value.valType == ValueType.VAL_INT;
}

pub fn asInt(value: Value) i64 {
    return value.as.integer;
}

pub fn intValue(i: i64) Value {
    return Value{ .valType = ValueType.VAL_INT, .as = .{ .integer = i } };
}

pub fn isString(value: Value) bool {
    return value.valType == ValueType.VAL_STRING;
}

pub fn asString(value: Value) []const u8 {
    return value.as.string;
}

pub fn stringValue(s: []const u8) Value {
    return Value{ .valType = ValueType.VAL_STRING, .as = .{ .string = s } };
}

pub fn valuesEqual(a: Value, b: Value) bool {
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
            std.debug.print("{s}\n", .{toPrint});
        },
        ValueType.VAL_INT => std.debug.print("{d}\n", .{asInt(value)}),
        ValueType.VAL_STRING => std.debug.print("{s}\n", .{asString(value)}),
        else => std.debug.print("Unable to print value\n", .{}),
    }
}
