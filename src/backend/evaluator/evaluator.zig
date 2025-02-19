const std = @import("std");
const testing = std.testing;
const debug = std.debug;
const assert = debug.assert;

const Objects = @import("../types/object.zig");
const Object = Objects.Object;
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

    pub fn evaluateProgram(self: *Self, program: *AST.Program, env: *Environment) EvaluatorError!*Object {
        var val: *Object = undefined;
        for (program.statements.items) |stmt| {
            const evaluation = self.evaluateStatement(@constCast(&stmt), env) catch return EvaluatorError.UnsupportedObject;
            switch (evaluation.*) {
                .returnObject => |ret| {
                    ret.toString();
                    return ret.value;
                },
                .err => {
                    evaluation.toString();
                    return evaluation;
                },
                else => val = evaluation,
            }
        }
        val.toString();
        return val;
    }

    fn evaluateStatement(self: *Self, statement: *AST.Statement, env: *Environment) EvaluatorError!*Object {
        switch (statement.*) {
            .Expr => |expr| return self.evaluateExpression(expr.expression, env),
            .Block => |*block| return self.evaluateBlockStatement(block, env),
            .Return => |ret| {
                const val = try self.evaluateExpression(ret.returnValue, env);
                switch (val.*) {
                    .err => return val,
                    else => {},
                }

                const objPtr = self.alloc.create(Object) catch return EvaluatorError.MemoryAllocation;
                objPtr.* = Object{ .returnObject = Objects.Return{
                    .value = val,
                } };

                return objPtr;
            },
            .Let => |stmt| {
                const val = try self.evaluateExpression(stmt.value, env);
                switch (val.*) {
                    .err => return val,
                    else => {},
                }

                env.*.set(stmt.name.value, val) catch return EvaluatorError.MemoryAllocation;
                return Objects.Null.new(self.alloc);
            },
            else => unreachable,
        }
    }

    fn evaluateExpression(self: *Evaluator, expression: *const AST.Expression, env: *Environment) EvaluatorError!*Object {
        switch (expression.*) {
            .Integer => |int| return try Objects.Integer.new(self.alloc, int.value),
            .Bool => |expr| return try Objects.Boolean.new(self.alloc, expr.value),
            .Prefix => |expr| {
                const right = try self.evaluateExpression(expr.right, env);
                switch (right.*) {
                    .err => return right,
                    else => {},
                }
                return self.evaluatePrefixExpression(expr.operator, right, env);
            },
            .Infix => |expr| {
                const left = try self.evaluateExpression(expr.left, env);
                switch (left.*) {
                    .err => return left,
                    else => {},
                }
                const right = try self.evaluateExpression(expr.right, env);
                switch (right.*) {
                    .err => return right,
                    else => {},
                }
                return self.evaluateInfixExpression(expr.operator, left, right, env);
            },
            .If => |stmt| return self.evaluateIfExpression(@constCast(&stmt), env),
            .Ident => |ident| return self.evaluateIdentifier(@constCast(&ident), env),
            .Function => |func| return try Objects.Function.new(self.alloc, func.parameters, func.body, env),
            .Call => |call| {
                const function = try self.evaluateExpression(call.function, env);
                switch (function.*) {
                    .err => return function,
                    else => {},
                }

                var args = std.ArrayList(*Object).init(self.alloc);
                for (call.args.items) |arg| {
                    const evaledArg = try self.evaluateExpression(&arg, env);
                    switch (evaledArg.*) {
                        .err => return evaledArg,
                        else => {},
                    }
                    args.append(evaledArg) catch return EvaluatorError.MemoryAllocation;
                }
                return try self.applyFunction(function, args);
            },
            .String => |s| return Objects.String.new(self.alloc, s.value),
        }
    }

    fn evaluateBangOperatorExpression(self: *Evaluator, right: *Object, _: *Environment) EvaluatorError!*Object {
        switch (right.*) {
            .boolean => |boolean| {
                if (boolean.value) {
                    return try Objects.Boolean.new(self.alloc, false);
                } else {
                    return try Objects.Boolean.new(self.alloc, true);
                }
            },
            .nullObject => return try Objects.Null.new(self.alloc),
            else => return try Objects.Boolean.new(self.alloc, false),
        }
    }
    fn evaluateMinusPrefixOperatorExpression(self: *Evaluator, right: *Object, _: *Environment) EvaluatorError!*Object {
        switch (right.*) {
            .integer => |integer| return try Objects.Integer.new(self.alloc, -integer.value),
            else => return try Objects.Error.new(self.alloc, "unable to evalue minus prefix operator", .{}),
        }
    }

    fn evaluatePrefixExpression(self: *Evaluator, operator: AST.PrefixOperator, right: *Object, env: *Environment) EvaluatorError!*Object {
        switch (operator) {
            .BANG => return self.evaluateBangOperatorExpression(right, env),
            .MINUS => return try self.evaluateMinusPrefixOperatorExpression(right, env),
            //else => return try Objects.Error.new(self.alloc, "unable to evaluate prefix expression."),
        }
    }
    fn evaluateIntegerInfixExpression(self: *Evaluator, operator: AST.InfixOperator, left: *Objects.Integer, right: *Objects.Integer) EvaluatorError!*Object {
        const leftVal = left.*.value;
        const rightVal = right.*.value;
        switch (operator) {
            .PLUS => return Objects.Integer.new(self.alloc, leftVal + rightVal),
            .MINUS => return Objects.Integer.new(self.alloc, leftVal - rightVal),
            .MULTIPLY => return Objects.Integer.new(self.alloc, leftVal * rightVal),
            .DIVIDE => return Objects.Integer.new(self.alloc, @divExact(leftVal, rightVal)),
            .LESS_THAN => return Objects.Boolean.new(self.alloc, leftVal < rightVal),
            .GREATER_THAN => return Objects.Boolean.new(self.alloc, leftVal > rightVal),
            .EQUAL => return Objects.Boolean.new(self.alloc, leftVal == rightVal),
            .NOT_EQUAL => return Objects.Boolean.new(self.alloc, leftVal != rightVal),
        }
    }

    fn evaluateInfixExpression(self: *Evaluator, operator: AST.InfixOperator, left: *Object, right: *Object, _: *Environment) EvaluatorError!*Object {
        switch (left.*) {
            .integer => |leftInt| {
                switch (right.*) {
                    .integer => |rightInt| {
                        return try self.evaluateIntegerInfixExpression(operator, @constCast(&leftInt), @constCast(&rightInt));
                    },
                    else => return Objects.Error.new(self.alloc, "type mismatch: {s} {s} {s}", .{ leftInt.typeName(), operator.toString(), right.typeName() }),
                }
            },
            else => {
                switch (operator) {
                    .EQUAL => return Objects.Boolean.new(self.alloc, Objects.compareObjects(left, right)),
                    .NOT_EQUAL => return Objects.Boolean.new(self.alloc, !Objects.compareObjects(left, right)),
                    else => {
                        if (std.mem.eql(u8, @tagName(left.*), @tagName(right.*))) {
                            return Objects.Error.new(self.alloc, "unknown operator: {s} {s} {s}", .{ left.typeName(), operator.toString(), right.typeName() });
                        } else {
                            return Objects.Error.new(self.alloc, "type mismatch: {s} {s} {s}", .{ left.typeName(), operator.toString(), right.typeName() });
                        }
                    },
                }
            },
        }
    }

    fn evaluateIfExpression(self: *Evaluator, expr: *AST.IfExpression, env: *Environment) EvaluatorError!*Object {
        const condition = try self.evaluateExpression(expr.condition, env);
        switch (condition.*) {
            .err => return condition,
            else => {},
        }

        if (isTruthy(condition)) {
            return self.evaluateBlockStatement(&expr.*.consequence, env);
        } else if (expr.alternative != null) {
            return self.evaluateBlockStatement(&expr.*.alternative.?, env);
        } else {
            return try Objects.Null.new(self.alloc);
        }
    }

    fn isTruthy(val: *Object) bool {
        switch (val.*) {
            .boolean => |boolean| return boolean.value,
            .nullObject => return false,
            else => return true,
        }
    }

    fn applyFunction(self: *Evaluator, function: *Object, args: std.ArrayList(*Object)) !*Object {
        switch (function.*) {
            .function => |func| {
                if (func.params.items.len != args.items.len) {
                    return Objects.Error.new(self.alloc, "wrong number of arguments: want {any} got {any}", .{ func.params.items.len, args.items.len });
                }
                const extendedEnv = try self.extendFunctionEnv(&func, args);
                const evaluated = try self.evaluateBlockStatement(func.body, extendedEnv);
                return unwrapReturnObject(evaluated);
            },
            else => return Objects.Error.new(self.alloc, "not a function: {s}", .{function.typeName()}),
        }
    }

    fn extendFunctionEnv(self: *Evaluator, func: *const Objects.Function, args: std.ArrayList(*Object)) EvaluatorError!*Environment {
        const envPtr = self.alloc.create(Environment) catch return EvaluatorError.MemoryAllocation;
        envPtr.* = Environment.newEnclosed(func.*.env);
        var idx: usize = 0;
        for (func.params.items) |param| {
            envPtr.*.set(param.value, args.items[idx]) catch return EvaluatorError.MemoryAllocation;
            idx += 1;
        }
        return envPtr;
    }

    fn unwrapReturnObject(obj: *Object) *Object {
        switch (obj.*) {
            .returnObject => |ret| return ret.value,
            else => return obj,
        }
    }

    fn evaluateIdentifier(self: *Evaluator, ident: *AST.Identifier, env: *Environment) EvaluatorError!*Object {
        const val = env.get(ident.value);
        if (val) |value| {
            return value;
        } else {
            return try Objects.Error.new(self.alloc, "object doesnt exist in the hash store.", .{});
        }
    }

    fn evaluateBlockStatement(self: *Evaluator, block: *AST.BlockStatement, env: *Environment) EvaluatorError!*Object {
        var result: *Object = try Objects.Null.new(self.alloc);
        var i: usize = 0;
        while (i < block.statements.items.len) : (i += 1) {
            const evaluated = try self.evaluateStatement(&block.statements.items[i], env);
            switch (evaluated.*) {
                .returnObject => return evaluated,
                .err => return evaluated,
                else => result = evaluated,
            }
        }
        return result;
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
        .{ .input = "return 10;", .expected = Objects.Integer.new(testingAlloc, 10) },
        .{ .input = "return 10; 9;", .expected = Objects.Integer.new(testingAlloc, 10) },
        .{ .input = "return 2 * 5; 6;", .expected = Objects.Integer.new(testingAlloc, 10) },
        .{ .input = "9; return 2 * 5; 6;", .expected = Objects.Integer.new(testingAlloc, 10) },
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
