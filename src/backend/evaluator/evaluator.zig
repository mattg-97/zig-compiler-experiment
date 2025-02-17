const std = @import("std");
const testing = std.testing;
const debug = std.debug;
const assert = debug.assert;

const Value = @import("../types/value.zig");
const Lexer = @import("../../frontend/lexer.zig");
const Parser = @import("../../frontend/parser/parser.zig");
const AST = @import("../../frontend/parser/ast.zig");

pub const Evaluator = struct {
    alloc: std.mem.Allocator,
    pub fn init(alloc: std.mem.Allocator) !*Evaluator {
        const evaluator = try alloc.create(Evaluator);
        evaluator.* = .{
            .alloc = alloc,
        };
        return evaluator;
    }
    fn evaluateIntegerExpression(_: *Evaluator, expr: AST.IntegerExpression) Value {
        return Value.intValue(expr.value);
    }

    fn evaluateBooleanExpression(_: *Evaluator, expr: AST.BooleanExpression) Value {
        return Value.boolValue(expr.value);
    }

    fn evaluateBangOperatorExpression(_: *Evaluator, right: Value) Value {
        switch (right.as) {
            .boolean => |bl| {
                return Value.boolValue(!bl);
            },
            .nil => return Value.boolValue(true),
            else => return Value.boolValue(false),
        }
    }
    fn evaluateMinusPrefixOperatorExpression(_: *Evaluator, right: Value) Value {
        if (!right.isInt()) return Value.nilValue();
        return Value.intValue(-right.asInt());
    }

    fn evaluatePrefixExpression(self: *Evaluator, operator: []const u8, right: Value) Value {
        if (std.mem.eql(u8, operator, "!")) {
            return self.evaluateBangOperatorExpression(right);
        } else if (std.mem.eql(u8, operator, "-")) {
            return self.evaluateMinusPrefixOperatorExpression(right);
        } else {
            return Value.nilValue();
        }
    }
    fn evaluateIntegerInfixExpression(_: *Evaluator, operator: []const u8, left: Value, right: Value) Value {
        const leftVal = left.asInt();
        const rightVal = right.asInt();
        if (std.mem.eql(u8, operator, "+")) return Value.intValue(leftVal + rightVal);
        if (std.mem.eql(u8, operator, "-")) return Value.intValue(leftVal - rightVal);
        if (std.mem.eql(u8, operator, "*")) return Value.intValue(leftVal * rightVal);
        if (std.mem.eql(u8, operator, "/")) return Value.intValue(@divExact(leftVal, rightVal));
        if (std.mem.eql(u8, operator, "<")) return Value.boolValue(leftVal < rightVal);
        if (std.mem.eql(u8, operator, ">")) return Value.boolValue(leftVal > rightVal);
        if (std.mem.eql(u8, operator, "==")) return Value.boolValue(leftVal == rightVal);
        if (std.mem.eql(u8, operator, "!=")) return Value.boolValue(leftVal != rightVal);
        return Value.nilValue();
    }

    fn evaluateInfixExpression(self: *Evaluator, operator: []const u8, left: Value, right: Value) Value {
        if (std.mem.eql(u8, operator, "==")) return Value.boolValue(Value.valuesEqual(left, right));
        if (std.mem.eql(u8, operator, "!=")) return Value.boolValue(!Value.valuesEqual(left, right));
        if (left.isInt() and right.isInt()) return self.evaluateIntegerInfixExpression(operator, left, right);
        return Value.nilValue();
    }

    fn evaluateIfExpression(self: *Evaluator, expr: AST.IfExpression) Value {
        const condition = self.evaluateExpression(expr.condition.*);
        if (isTruthy(condition)) {
            return self.evaluateStatement(expr.consequence.*);
        } else if (expr.alternative != null) {
            return self.evaluateStatement(expr.alternative.?.*);
        } else {
            return Value.nilValue();
        }
    }

    fn isTruthy(val: Value) bool {
        if (val.isBool()) {
            if (val.asBool()) return true;
            if (!val.asBool()) return false;
        }
        if (val.isNil()) return false;
        return true;
    }

    fn evaluateExpression(self: *Evaluator, expression: AST.Expression) Value {
        switch (expression) {
            .Integer => |int| return self.evaluateIntegerExpression(int),
            .Bool => |expr| return self.evaluateBooleanExpression(expr),
            .Prefix => |expr| {
                const right = self.evaluateExpression(expr.right.*);
                return self.evaluatePrefixExpression(expr.operator, right);
            },
            .Infix => |expr| {
                const left = self.evaluateExpression(expr.left.*);
                const right = self.evaluateExpression(expr.right.*);
                return self.evaluateInfixExpression(expr.operator, left, right);
            },
            .If => |stmt| return self.evaluateIfExpression(stmt),
            else => unreachable,
        }
    }

    fn evaluateExpressionStatement(self: *Evaluator, statement: AST.ExpressionStatement) Value {
        return self.evaluateExpression(statement.expression.*);
    }

    fn evaluateStatement(self: *Evaluator, statement: AST.Statement) Value {
        switch (statement) {
            .Expr => |expr| return self.evaluateExpressionStatement(expr),
            .Block => |block| {
                var result: Value = undefined;
                for (block.statements.items) |stmt| {
                    result = self.evaluateStatement(stmt.*);
                }
                return result;
            },
            else => unreachable,
        }
    }

    pub fn evaluate(self: *Evaluator, program: *AST.Program) Value {
        var val: Value = undefined;
        for (program.statements.items) |stmt| {
            val = self.evaluateStatement(stmt);
        }
        std.debug.print("Value is: ", .{});
        Value.printValue(val);
        std.debug.print("\n", .{});
        return val;
    }
};

fn testEval(alloc: std.mem.Allocator, input: []const u8) !Value {
    const lexer = try Lexer.init(alloc, input);
    try lexer.tokenize();

    const parser = try Parser.init(alloc, lexer);
    const program = try parser.parseProgram();

    const evaluator = try Evaluator.init(alloc);
    return evaluator.evaluate(program);
}

fn testIntegerValue(value: Value, expected: i64) bool {
    if (!value.isInt()) return false;
    return (value.asInt() == expected);
}

test "test evaluation of integer expression" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: i64,
    };

    const tests = [_]TestType{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "10;", .expected = 10 },
        .{ .input = "-5;", .expected = -5 },
        .{ .input = "-10;", .expected = -10 },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        assert(val.asInt() == tt.expected);
    }
}

test "test evaluation of boolean expression" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: bool,
    };

    const tests = [_]TestType{
        .{ .input = "true;", .expected = true },
        .{ .input = "false;", .expected = false },
        .{ .input = "1 < 2;", .expected = true },
        .{ .input = "1 > 2;", .expected = false },
        .{ .input = "1 < 1;", .expected = false },
        .{ .input = "1 == 1;", .expected = true },
        .{ .input = "1 != 1;", .expected = false },
        .{ .input = "1 != 2;", .expected = true },
        .{ .input = "1 == 2;", .expected = false },
        .{ .input = "true == true;", .expected = true },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        assert(val.asBool() == tt.expected);
    }
}

test "test evaluation of prefix expression" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: bool,
    };

    const tests = [_]TestType{
        .{ .input = "!true;", .expected = false },
        .{ .input = "!false;", .expected = true },
        .{ .input = "!5;", .expected = false },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        assert(val.asBool() == tt.expected);
    }
}

test "test evaluation of infix expressions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: i64,
    };

    const tests = [_]TestType{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "5 + 5 + 5 + 5 - 10;", .expected = 10 },
        .{ .input = "-50 + 100 + -50;", .expected = 0 },
        .{ .input = "2 * (5 + 10);", .expected = 30 },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        std.debug.print("VAL: {any}\n", .{val});
        Value.printValue(val);
        std.debug.print("\n", .{});
        assert(val.asInt() == tt.expected);
    }
}

test "test if else expression" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: ?i64,
    };

    const tests = [_]TestType{
        .{ .input = "if (true) { 10; };", .expected = 10 },
        .{ .input = "if (false) { 10; };", .expected = null },
        .{ .input = "if (1 > 2) { 10; } else { 20; };", .expected = 20 },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        std.debug.print("VAL: {any}\n", .{val});
        Value.printValue(val);
        std.debug.print("\n", .{});
        if (val.isInt()) assert(val.asInt() == tt.expected);
    }
}
