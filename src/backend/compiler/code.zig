const Code = @This();

const std = @import("std");
const testing = std.testing;

const bits = @import("./bits.zig");

const Instructions = []u8;
const Op = u8;

const OpCode = enum(Op) {
    OpConstant,
    pub fn toDefinition(self: OpCode) Definition {
        const defintion = switch (self) {
            .OpConstant => {
                var buf = [_]u16{2};
                const widths = buf[0..1];
                .{ .name = "OpConstant", .operandWidths = widths };
            },
        };
        return defintion;
    }
};

const Definition = struct {
    name: []const u8,
    operandWidths: []u16,
};

pub fn Make(op: OpCode, operands: []const u16) []u8 {
    const def = op.toDefinition();
    const instructionLen = def.operandWidths.len;
    var tempArr = [_]u8{0} ** std.math.maxInt(u8);
    var instruction = tempArr[0..instructionLen];
    instruction[0] = @intFromEnum(op);
    var offset: usize = 1;
    for (operands, 0..) |operand, i| {
        const width = def.operandWidths[i];
        switch (width) {
            2 => {
                bits.PutUint16(&instruction[offset..], @as(u16, operand));
            },
            else => {},
        }
        offset += width;
    }
    return instruction;
}

fn arrayToSlice(comptime T: type, comptime arr: []T) []T {
    return arr[0..arr.len];
}

test "test make" {
    const TestCase = struct {
        op: OpCode,
        operands: []const u16,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        .{ .op = .OpConstant, .operands = &[_]u16{65534}, .expected = &[_]u8{ @as(u8, @intFromEnum(OpCode.OpConstant)), 255, 254 } },
    };

    for (cases) |case| {
        const instruction = Make(case.op, case.operands);

        try testing.expectEqual(instruction.len, case.expected.len);
        for (case.expected, 0..) |byte, idx| {
            try testing.expectEqual(instruction[idx], byte);
        }
    }
}
