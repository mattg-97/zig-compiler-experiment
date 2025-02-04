const Value = @This();

const std = @import("std");

const ValueType = enum {
    VAL_BOOL,
    VAL_INT,
    VAL_FLOATING,
    VAL_STRING,
    VAL_NIL,
};
