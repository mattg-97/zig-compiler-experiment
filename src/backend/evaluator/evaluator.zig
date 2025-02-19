const std = @import("std");
const testing = std.testing;
const debug = std.debug;
const assert = debug.assert;

const Object = @import("../types/object.zig").Object;
const Lexer = @import("../../frontend/lexer.zig");
const Parser = @import("../../frontend/parser/parser.zig");
const AST = @import("../../frontend/parser/ast.zig");
const Environment = @import("../environment/environment.zig");
pub const EvaluatorError = error{
    MemoryAllocation,
    UnsupportedObject,
};

pub const Evaluator = struct {
    alloc: std.mem.Allocator,
    const Self = @This();
    pub fn init(alloc: std.mem.Allocator) Self {
        return .{
            .alloc = alloc,
        };
    }

    fn evaluateNode(self: *Self, node: AST.Node, env: *Environment) EvaluatorError!*Object {
        switch (node) {
            .program => |program| return try self.evaluateProgram(program, env),
            .statement => |stmt| return try self.evaluateStatement(stmt.*, env),
            .expression => |expr| return try self.evaluateExpression(expr.*, env),
        }
    }

    fn evaluateIntegerExpression(_: *Evaluator, expr: AST.IntegerExpression, _: *Environment) Object {
        return Object.intObject(expr.value);
    }

    fn evaluateBooleanExpression(_: *Evaluator, expr: AST.BooleanExpression, _: *Environment) Object {
        return Object.boolObject(expr.value);
    }

    fn evaluateBangOperatorExpression(_: *Evaluator, right: Object, _: *Environment) Object {
        switch (right.as) {
            .boolean => |bl| {
                return Object.boolObject(!bl);
            },
            .nil => return Object.boolObject(true),
            else => return Object.boolObject(false),
        }
    }
    fn evaluateMinusPrefixOperatorExpression(self: *Evaluator, right: Object, _: *Environment) Object {
        if (!right.isInt()) {
            const message = std.fmt.allocPrint(self.alloc, "unknown operator: -{s}\n", .{right.valType.toString()}) catch "Unable to allocate print error message";
            const err: Object.Error = .{ .message = message };
            return Object.errorObject(err);
        }
        return Object.intObject(-right.asInt());
    }

    fn evaluatePrefixExpression(self: *Evaluator, operator: AST.PrefixOperator, right: Object, env: *Environment) Object {
        switch (operator) {
            .BANG => return self.evaluateBangOperatorExpression(right, env),
            .MINUS => return self.evaluateMinusPrefixOperatorExpression(right, env),
        }
    }
    fn evaluateIntegerInfixExpression(_: *Evaluator, operator: AST.InfixOperator, left: Object, right: Object, _: *Environment) Object {
        const leftVal = left.asInt();
        const rightVal = right.asInt();
        switch (operator) {
            .PLUS => return Object.intObject(leftVal + rightVal),
            .MINUS => return Object.intObject(leftVal - rightVal),
            .MULTIPLY => return Object.intObject(leftVal * rightVal),
            .DIVIDE => return Object.intObject(@divExact(leftVal, rightVal)),
            .LESS_THAN => return Object.boolObject(leftVal < rightVal),
            .GREATER_THAN => return Object.boolObject(leftVal > rightVal),
            .EQUAL => return Object.boolObject(leftVal == rightVal),
            .NOT_EQUAL => return Object.boolObject(leftVal != rightVal),
        }
    }

    fn evaluateInfixExpression(self: *Evaluator, operator: AST.InfixOperator, left: Object, right: Object, env: *Environment) !Object {
        if (left.valType != right.valType) {
            const message = std.fmt.allocPrint(self.alloc, "type mismatch: {s} {s} {s}", .{ left.valType.toString(), operator.toString(), right.valType.toString() }) catch "Unable to allocate error string";
            const err: Object.Error = .{ .message = message };
            return Object.errorObject(err);
        }
        switch (operator) {
            .EQUAL => return Object.boolObject(Object.ObjectsEqual(left, right)),
            .NOT_EQUAL => return Object.boolObject(!Object.ObjectsEqual(left, right)),
            else => return self.evaluateIntegerInfixExpression(operator, left, right, env),
        }
    }

    fn evaluateIfExpression(self: *Evaluator, expr: AST.IfExpression, env: *Environment) !Object {
        const condition = try self.evaluateExpression(expr.condition.*, env);
        if (condition.isError()) return condition;
        if (isTruthy(condition)) {
            return self.evaluateBlockStatement(expr.consequence, env);
        } else if (expr.alternative != null) {
            return self.evaluateBlockStatement(expr.alternative.?, env);
        } else {
            return Object.nilObject();
        }
    }

    fn isTruthy(val: Object) bool {
        if (val.isBool()) {
            if (val.asBool()) return true;
            if (!val.asBool()) return false;
        }
        if (val.isNil()) return false;
        return true;
    }

    fn applyFunction(self: *Evaluator, func: Object, args: []Object) !Object {
        if (!func.isFunc()) return Object.errorObject(Object.Error{ .message = "Unexpected Object, should be func." });
        if (func.asFunc().params.len != args.len) return Object.errorObject(Object.Error{ .message = "Arguments should be same size as params\n" });
        const extendedEnv = try self.extendFunctionEnv(func.asFunc(), args);
        const evaluated = try self.evaluateBlockStatement(func.asFunc().body, extendedEnv);
        return unwrapReturnObject(evaluated);
    }

    fn extendFunctionEnv(_: *Evaluator, func: Object.Function, args: []Object) !*Environment {
        var env = try Environment.newEnclosed(func.env);
        var idx: usize = 0;
        for (func.params) |param| {
            _ = try env.set(param.value, args[idx]);
            idx += 1;
        }
        return env;
    }

    fn unwrapReturnObject(object: Object) Object {
        if (object.isReturn()) return object.returnObject();
        return object;
    }

    fn evaluateIdentifier(_: *Evaluator, ident: AST.Identifier, env: *Environment) !Object {
        const val = env.get(ident.value);
        if (val == null) return Object.errorObject(Object.Error{ .message = "Object doesnt exist in hash map store" });
        val.?.printObject();
        return val.?;
    }

    fn evaluateExpression(self: *Evaluator, expression: AST.Expression, env: *Environment) !Object {
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
                const funcVal = Object.Function{
                    .body = body,
                    .params = params.items,
                    .env = env,
                };
                return Object.funcObject(funcVal);
            },
            .Call => |call| {
                const function = try self.evaluateExpression(call.function.*, env);
                if (function.isError()) return function;
                const args = try self.evaluateExpressions(call.args.items, env);
                if (args.len == 1 and args[0].isError()) return args[0];
                const funcResult = try self.applyFunction(function, args);
                return funcResult;
            },
        }
    }

    fn evaluateExpressions(self: *Evaluator, exps: []AST.Expression, env: *Environment) anyerror![]Object {
        var vals = std.ArrayList(Object).init(self.alloc);
        for (exps) |expr| {
            const evaluated = try self.evaluateExpression(expr, env);
            if (evaluated.isError()) {
                vals.clearAndFree();
                try vals.append(evaluated);
                return vals.items;
            }
            try vals.append(evaluated);
        }
        return vals.items;
    }

    fn evaluateBlockStatement(self: *Evaluator, block: AST.BlockStatement, env: *Environment) !Object {
        var result: Object = undefined;
        for (block.statements.items) |stmt| {
            result = try self.evaluateStatement(stmt, env);
            if (!result.isNil()) {
                if (result.valType == Object.ObjectType.VAL_RETURN or result.valType == Object.ObjectType.ERROR) return result;
            }
        }
        return result;
    }

    fn evaluateExpressionStatement(self: *Evaluator, statement: AST.ExpressionStatement, env: *Environment) !Object {
        const stmt = try self.evaluateExpression(statement.expression.*, env);
        return stmt;
    }

    fn evaluateStatement(self: *Evaluator, statement: AST.Statement, env: *Environment) anyerror!Object {
        switch (statement) {
            .Expr => |expr| return self.evaluateExpressionStatement(expr, env),
            .Block => |block| return self.evaluateBlockStatement(block, env),
            .Return => |ret| {
                const val = try self.evaluateExpression(ret.returnValue.*, env);
                if (val.isError()) return val;
                return Object.returnObject(val);
            },
            .Let => |stmt| {
                const val = try self.evaluateExpression(stmt.value.*, env);
                if (val.isError()) return val;
                if (env.set(stmt.name.value, val)) |object| {
                    return object;
                } else |_| {
                    @panic("Unable to add key to hash store");
                }
            },
            else => unreachable,
        }
    }

    pub fn evaluateProgram(self: *Self, program: *AST.Program, env: *Environment) EvaluatorError!*Object {
        var val: Object = Object.nilObject();
        for (program.statements.items) |stmt| {
            const evaluation = self.evaluateStatement(stmt, env) catch return EvaluatorError.UnsupportedObject;
            switch (evaluation) {
                .returnObject => |ret| return ret.value,
                .err => return evaluation,
                else => val = evaluation,
            }
        }
        std.debug.print("Object is: ", .{});
        Object.printObject(val);
        std.debug.print("\n", .{});
        return val;
    }
};

fn testEval(alloc: std.mem.Allocator, input: []const u8) !Object {
    const lexer = try Lexer.init(alloc, input);
    try lexer.tokenize();

    const parser = try Parser.init(alloc, lexer);
    const program = try parser.parseProgram();

    const environment = try Environment.init(alloc);
    const evaluator = try Evaluator.init(alloc);
    return evaluator.evaluate(program, environment);
}

fn testIntegerObject(object: Object, expected: i64) bool {
    if (!object.isInt()) return false;
    return (object.asInt() == expected);
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
        expected: Object,
    };

    const tests = [_]TestType{
        .{ .input = "return 10;", .expected = Object.intObject(10) },
        .{ .input = "return 10; 9;", .expected = Object.intObject(10) },
        .{ .input = "return 2 * 5; 6;", .expected = Object.intObject(10) },
        .{ .input = "9; return 2 * 5; 6;", .expected = Object.intObject(10) },
    };

    for (tests) |tt| {
        const val = try testEval(testingAlloc, tt.input);
        try testing.expect(Object.ObjectsEqual(val, tt.expected));
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
    try testing.expectEqualStrings(val.asFunc().params[0].*.Object, "x");
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
            ret.printObject();
            try testing.expect(ret.isInt());
        }
        if (val.isInt()) {
            try testing.expect(val.isInt());
            try testing.expectEqual(val.asInt(), tt.expected);
        }
    }
}
