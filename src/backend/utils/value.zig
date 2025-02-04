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
