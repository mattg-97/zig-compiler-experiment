const std = @import("std");
const testing = std.testing;
const debug = std.debug;
const assert = debug.assert;

const Value = @import("../types/value.zig");
const Lexer = @import("../../frontend/lexer.zig");
const Parser = @import("../../frontend/parser/parser.zig");
const AST = @import("../../frontend/parser/ast.zig");
const Environment = @import("../environment/environment.zig");

pub const Evaluator = struct {
    alloc: std.mem.Allocator,
    pub fn init(alloc: std.mem.Allocator) !*Evaluator {
        const evaluator = try alloc.create(Evaluator);
        evaluator.* = .{
            .alloc = alloc,
        };
        return evaluator;
    }
    fn evaluateIntegerExpression(_: *Evaluator, expr: AST.IntegerExpression, _: *Environment) Value {
        return Value.intValue(expr.value);
    }

    fn evaluateBooleanExpression(_: *Evaluator, expr: AST.BooleanExpression, _: *Environment) Value {
        return Value.boolValue(expr.value);
    }

    fn evaluateBangOperatorExpression(_: *Evaluator, right: Value, _: *Environment) Value {
        switch (right.as) {
            .boolean => |bl| {
                return Value.boolValue(!bl);
            },
            .nil => return Value.boolValue(true),
            else => return Value.boolValue(false),
        }
    }
    fn evaluateMinusPrefixOperatorExpression(self: *Evaluator, right: Value, _: *Environment) Value {
        if (!right.isInt()) {
            const message = std.fmt.allocPrint(self.alloc, "unknown operator: -{s}\n", .{right.valType.toString()}) catch "Unable to allocate print error message";
            const err: Value.Error = .{ .message = message };
            return Value.errorValue(err);
        }
        return Value.intValue(-right.asInt());
    }

    fn evaluatePrefixExpression(self: *Evaluator, operator: AST.PrefixOperator, right: Value, env: *Environment) Value {
        switch (operator) {
            .BANG => return self.evaluateBangOperatorExpression(right, env),
            .MINUS => return self.evaluateMinusPrefixOperatorExpression(right, env),
        }
    }
    fn evaluateIntegerInfixExpression(_: *Evaluator, operator: AST.InfixOperator, left: Value, right: Value, _: *Environment) Value {
        const leftVal = left.asInt();
        const rightVal = right.asInt();
        switch (operator) {
            .PLUS => return Value.intValue(leftVal + rightVal),
            .MINUS => return Value.intValue(leftVal - rightVal),
            .MULTIPLY => return Value.intValue(leftVal * rightVal),
            .DIVIDE => return Value.intValue(@divExact(leftVal, rightVal)),
            .LESS_THAN => return Value.boolValue(leftVal < rightVal),
            .GREATER_THAN => return Value.boolValue(leftVal > rightVal),
            .EQUAL => return Value.boolValue(leftVal == rightVal),
            .NOT_EQUAL => return Value.boolValue(leftVal != rightVal),
        }
    }

    fn evaluateInfixExpression(self: *Evaluator, operator: AST.InfixOperator, left: Value, right: Value, env: *Environment) !Value {
        if (left.valType != right.valType) {
            const message = std.fmt.allocPrint(self.alloc, "type mismatch: {s} {s} {s}", .{ left.valType.toString(), operator.toString(), right.valType.toString() }) catch "Unable to allocate error string";
            const err: Value.Error = .{ .message = message };
            return Value.errorValue(err);
        }
        switch (operator) {
            .EQUAL => return Value.boolValue(Value.valuesEqual(left, right)),
            .NOT_EQUAL => return Value.boolValue(!Value.valuesEqual(left, right)),
            else => return self.evaluateIntegerInfixExpression(operator, left, right, env),
        }
    }

    fn evaluateIfExpression(self: *Evaluator, expr: AST.IfExpression, env: *Environment) !Value {
        const condition = try self.evaluateExpression(expr.condition.*, env);
        if (condition.isError()) return condition;
        if (isTruthy(condition)) {
            return self.evaluateStatement(expr.consequence.*, env);
        } else if (expr.alternative != null) {
            return self.evaluateStatement(expr.alternative.?.*, env);
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

    fn applyFunction(self: *Evaluator, func: Value, args: []Value) !Value {
        if (!func.isFunc()) return Value.errorValue(Value.Error{ .message = "Unexpected value, should be func." });
        if (func.asFunc().params.len != args.len) return Value.errorValue(Value.Error{ .message = "Arguments should be same size as params\n" });
        const extendedEnv = try self.extendFunctionEnv(func.asFunc(), args);
        const evaluated = try self.evaluateBlockStatement(func.asFunc().body, extendedEnv);
        return unwrapReturnValue(evaluated);
    }

    fn extendFunctionEnv(_: *Evaluator, func: Value.Function, args: []Value) !*Environment {
        var env = try Environment.newEnclosed(func.env);
        var idx: usize = 0;
        for (func.params) |param| {
            _ = try env.set(param.*.value, args[idx]);
            idx += 1;
        }
        return env;
    }

    fn unwrapReturnValue(value: Value) Value {
        if (value.isReturn()) return value.returnValue();
        return value;
    }

    fn evaluateIdentifier(_: *Evaluator, ident: AST.Identifier, env: *Environment) !Value {
        const val = env.get(ident.value);
        if (val == null) return Value.errorValue(Value.Error{ .message = "Value doesnt exist in hash map store" });
        val.?.printValue();
        return val.?;
    }

    fn evaluateExpression(self: *Evaluator, expression: AST.Expression, env: *Environment) !Value {
        switch (expression) {
            .Integer => |int| return self.evaluateIntegerExpression(int, env),
            .Bool => |expr| return self.evaluateBooleanExpression(expr, env),
            .Prefix => |expr| {
                const right = try self.evaluateExpression(expr.right.*, env);
                if (right.isError()) return right;
                return self.evaluatePrefixExpression(expr.operator, right, env);
            },
            .Infix => |expr| {
                const left = try self.evaluateExpression(expr.left.*, env);
                if (left.isError()) return left;
                const right = try self.evaluateExpression(expr.right.*, env);
                if (right.isError()) return right;
                return self.evaluateInfixExpression(expr.operator, left, right, env);
            },
            .If => |stmt| return self.evaluateIfExpression(stmt, env),
            .Ident => |ident| return self.evaluateIdentifier(ident, env),
            .Function => |func| {
                const params = func.parameters;
                const body = func.body;
                const funcVal = Value.Function{
                    .body = body.*,
                    .params = params,
                    .env = env,
                };
                return Value.funcValue(funcVal);
            },
            .Call => |call| {
                const function = try self.evaluateExpression(call.function.*, env);
                if (function.isError()) return function;
                const args = try self.evaluateExpressions(call.args, env);
                if (args.len == 1 and args[0].isError()) return args[0];
                const funcResult = try self.applyFunction(function, args);
                return funcResult;
            },
        }
    }

    fn evaluateExpressions(self: *Evaluator, exps: []*AST.Expression, env: *Environment) anyerror![]Value {
        var vals = std.ArrayList(Value).init(self.alloc);
        for (exps) |expr| {
            const evaluated = try self.evaluateExpression(expr.*, env);
            if (evaluated.isError()) {
                vals.clearAndFree();
                try vals.append(evaluated);
                return vals.items;
            }
            try vals.append(evaluated);
        }
        return vals.items;
    }

    fn evaluateBlockStatement(self: *Evaluator, block: AST.BlockStatement, env: *Environment) !Value {
        var result: Value = undefined;
        for (block.statements.items) |stmt| {
            result = try self.evaluateStatement(stmt.*, env);
            if (!result.isNil()) {
                if (result.valType == Value.ValueType.VAL_RETURN or result.valType == Value.ValueType.ERROR) return result;
            }
        }
        return result;
    }

    fn evaluateExpressionStatement(self: *Evaluator, statement: AST.ExpressionStatement, env: *Environment) !Value {
        const stmt = try self.evaluateExpression(statement.expression.*, env);
        return stmt;
    }

    fn evaluateStatement(self: *Evaluator, statement: AST.Statement, env: *Environment) anyerror!Value {
        switch (statement) {
            .Expr => |expr| return self.evaluateExpressionStatement(expr, env),
            .Block => |block| return self.evaluateBlockStatement(block, env),
            .Return => |ret| {
                const val = try self.evaluateExpression(ret.returnValue.*, env);
                if (val.isError()) return val;
                return Value.returnValue(val);
            },
            .Let => |stmt| {
                const val = try self.evaluateExpression(stmt.value.*, env);
                if (val.isError()) return val;
                if (env.set(stmt.name.value, val)) |value| {
                    return value;
                } else |_| {
                    @panic("Unable to add key to hash store");
                }
            },
            else => unreachable,
        }
    }

    pub fn evaluate(self: *Evaluator, program: *AST.Program, env: *Environment) !Value {
        var val: Value = undefined;
        for (program.statements.items) |stmt| {
            val = try self.evaluateStatement(stmt, env);
            if (val.isReturn()) return val.asReturn();
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

    const environment = try Environment.init(alloc);
    const evaluator = try Evaluator.init(alloc);
    return evaluator.evaluate(program, environment);
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
        if (!val.isNil() and val.isInt()) assert(val.asInt() == tt.expected);
    }
}

test "test return statement" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: Value,
    };

    const tests = [_]TestType{
        .{ .input = "return 10;", .expected = Value.intValue(10) },
        .{ .input = "return 10; 9;", .expected = Value.intValue(10) },
        .{ .input = "return 2 * 5; 6;", .expected = Value.intValue(10) },
        .{ .input = "9; return 2 * 5; 6;", .expected = Value.intValue(10) },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        try testing.expect(Value.valuesEqual(val, tt.expected));
    }
}

test "test error handling" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: []const u8,
    };

    const tests = [_]TestType{
        .{ .input = "5 + true;", .expected = "type mismatch: INTEGER + BOOLEAN" },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        assert(val.isError());
        assert(std.mem.eql(u8, val.asError().message, tt.expected));
    }
}

test "test let statements" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const TestType = struct {
        input: []const u8,
        expected: i64,
    };

    const tests = [_]TestType{
        .{ .input = "let a = 5; a;", .expected = 5 },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        try testing.expect(val.isInt());
        try testing.expectEqual(val.asInt(), tt.expected);
    }
}

test "test functions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const input = "fn(x) { x + 2; };";
    const val = try testEval(testingAlloc, input);

    try testing.expect(val.isFunc());
    try testing.expect(val.asFunc().params.len == 1);
    try testing.expectEqualStrings(val.asFunc().params[0].*.value, "x");
}

test "test function applications" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();

    const TestType = struct {
        input: []const u8,
        expected: i64,
    };

    const tests = [_]TestType{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { return x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { return x + y; }; add(5, 10);", .expected = 15 },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        if (val.isReturn()) {
            try testing.expect(val.isReturn());
            const ret = val.asReturn();
            ret.printValue();
            try testing.expect(ret.isInt());
        }
        if (val.isInt()) {
            try testing.expect(val.isInt());
            try testing.expectEqual(val.asInt(), tt.expected);
        }
    }
}
