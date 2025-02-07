const Parser = @This();

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer.zig");
const Token = @import("../tokens.zig").Token;
const TokenType = @import("../tokens.zig").TokenType;
const AST = @import("ast.zig");

lexer: *Lexer,
current: usize,
current_token: Token,
peek_token: Token,
alloc: std.mem.Allocator,
errors: std.ArrayList([]const u8),
panic_mode: bool,
has_error: bool,
program: *AST.Program,

const ParsingError = error{
    UnexpectedTokenType,
    UnknownStatementType,
    PeekError,
};

fn peekError(self: *Parser, t: TokenType) !void {
    const msg = try std.fmt.allocPrint(self.alloc, "Expected token to be {s} but got {s} instead.\n", .{ std.enums.tagName(TokenType, t).?, std.enums.tagName(TokenType, self.peek_token.Type).? });
    std.debug.print("ERROR: {s}\n", .{msg});
    try self.errors.append(msg);
}

pub fn init(alloc: std.mem.Allocator, lexer: *Lexer) !*Parser {
    var p = try alloc.create(Parser);
    p.* = Parser{
        .lexer = lexer,
        .current_token = undefined,
        .peek_token = undefined,
        .alloc = alloc,
        .errors = std.ArrayList([]const u8).init(alloc),
        .panic_mode = false,
        .has_error = false,
        .current = 0,
        .program = try AST.Program.init(alloc),
    };
    p.nextToken();
    p.nextToken();
    return p;
}

// No longer needed after switching to arena allocator
//pub fn destroy(self: *Parser) void {
//    for (self.errors.items) |itm| {
//        self.alloc.free(itm);
//    }
//    self.errors.deinit();
//    self.alloc.destroy(self);
//}

fn nextToken(self: *Parser) void {
    self.current_token = self.peek_token;
    if (self.peek_token.Type != TokenType.EOF) {
        self.peek_token = self.lexer.tokens.items[self.current];
    }
    self.current += 1;
}

fn expectPeek(self: *Parser, tokenType: TokenType) !void {
    if (self.peek_token.Type == tokenType) {
        self.nextToken();
    } else {
        try self.peekError(tokenType);
    }
}

fn parseLetStatement(self: *Parser) !*AST.Statement {
    const letToken = self.current_token;
    try self.expectPeek(TokenType.IDENT);
    const identToken = self.current_token;
    try self.expectPeek(TokenType.ASSIGN);
    self.nextToken();
    const letIdent: AST.Identifier = .{ .token = identToken, .value = identToken.Literal };
    const letStmt: AST.LetStatement = .{ .token = letToken, .name = letIdent, .value = try self.parseExpression(AST.Precedence.LOWEST) };
    const stmt: AST.Statement = .{ .Let = letStmt };
    while (self.current_token.Type != TokenType.SEMICOLON) {
        self.nextToken();
    }
    const stmtPointer = try self.alloc.create(AST.Statement);
    stmtPointer.* = stmt;
    return stmtPointer;
}

fn parsePrintStatement(self: *Parser) !*AST.Statement {
    self.nextToken();
    const printStmt: AST.PrintStatement = .{ .token = self.current_token, .value = try self.parseExpression(AST.Precedence.LOWEST) };
    const stmt: AST.Statement = .{ .Print = printStmt };
    while (self.current_token.Type != TokenType.SEMICOLON) : (self.nextToken()) {}
    const stmtPointer = try self.alloc.create(AST.Statement);
    stmtPointer.* = stmt;
    return stmtPointer;
}

fn parseReturnStatement(self: *Parser) !*AST.Statement {
    const returnStmt: AST.ReturnStatement = .{ .token = self.current_token, .returnValue = try self.parseExpression(AST.Precedence.LOWEST) };
    const stmt: AST.Statement = .{ .Return = returnStmt };
    self.nextToken();
    while (self.current_token.Type != TokenType.SEMICOLON) : (self.nextToken()) {}
    const stmtPointer = try self.alloc.create(AST.Statement);
    stmtPointer.* = stmt;
    return stmtPointer;
}

fn parseIntegerExpression(self: *Parser) !*AST.Expression {
    const intVal = try std.fmt.parseInt(i64, self.current_token.Literal, 10);
    const intExpr: AST.IntegerExpression = .{ .token = self.current_token, .value = intVal };
    const expr: AST.Expression = .{ .Integer = intExpr };
    const exprPointer = try self.alloc.create(AST.Expression);
    exprPointer.* = expr;
    return exprPointer;
}

fn parsePrefix(self: *Parser) !*AST.Expression {
    const prefixToken = self.current_token;
    self.nextToken();
    const prefix: AST.PrefixExpression = .{ .token = prefixToken, .operator = prefixToken.Literal, .right = try self.parseExpression(AST.Precedence.PREFIX) };
    const expr: AST.Expression = .{ .Prefix = prefix };
    const exprPointer = try self.alloc.create(AST.Expression);
    exprPointer.* = expr;
    return exprPointer;
}

fn parseInfix(self: *Parser, left: *AST.Expression) !*AST.Expression {
    const infixToken = self.current_token;
    const prec = AST.getTokenPrecedence(&self.current_token);
    self.nextToken();
    const right = try self.parseExpression(prec);
    const infix: AST.InfixExpression = .{ .token = infixToken, .operator = infixToken.Literal, .left = left, .right = right };
    const expr: AST.Expression = .{ .Infix = infix };
    const exprPointer = try self.alloc.create(AST.Expression);
    exprPointer.* = expr;
    return exprPointer;
}

fn parseIdent(self: *Parser) !*AST.Expression {
    const ident: AST.Identifier = .{ .token = self.current_token, .value = self.current_token.Literal };
    const expr: AST.Expression = .{ .Ident = ident };
    const exprPointer = try self.alloc.create(AST.Expression);
    exprPointer.* = expr;
    return exprPointer;
}

fn parseExpression(self: *Parser, precedence: AST.Precedence) anyerror!*AST.Expression {
    var left: *AST.Expression = undefined;
    switch (self.current_token.Type) {
        TokenType.INT => left = try self.parseIntegerExpression(),
        TokenType.IDENT => left = try self.parseIdent(),
        TokenType.MINUS, TokenType.BANG => left = try self.parsePrefix(),
        else => std.debug.print("TOKEN: {s} type: {s}\n", .{ self.current_token.Literal, std.enums.tagName(TokenType, self.current_token.Type).? }),
    }
    while (self.peek_token.Type != TokenType.SEMICOLON and @intFromEnum(precedence) < @intFromEnum(AST.getTokenPrecedence(&self.peek_token))) {
        switch (self.peek_token.Type) {
            TokenType.PLUS, TokenType.ASTERISK, TokenType.SLASH, TokenType.GT, TokenType.LT, TokenType.EQ, TokenType.NOT_EQ, TokenType.MINUS, TokenType.ASSIGN => {
                self.nextToken();
                left = try self.parseInfix(left);
                break;
            },
            else => break,
        }
    }
    return left;
}

fn parseExpressionStatement(self: *Parser) !*AST.Statement {
    const exprStmt: AST.ExpressionStatement = .{ .token = self.current_token, .expression = try self.parseExpression(AST.Precedence.LOWEST) };
    const stmt: AST.Statement = .{ .Expr = exprStmt };
    while (self.current_token.Type != TokenType.SEMICOLON) : (self.nextToken()) {}
    const stmtPointer = try self.alloc.create(AST.Statement);
    stmtPointer.* = stmt;
    return stmtPointer;
}

fn parseStatement(self: *Parser) !*AST.Statement {
    return switch (self.current_token.Type) {
        TokenType.LET => try self.parseLetStatement(),
        TokenType.RETURN => try self.parseReturnStatement(),
        TokenType.PRINT => try self.parsePrintStatement(),
        else => try self.parseExpressionStatement(),
    };
}

pub fn parseProgram(self: *Parser) !*AST.Program {
    while (self.current_token.Type != TokenType.EOF) {
        const stmtPtr = try self.parseStatement();
        self.nextToken();
        try self.program.statements.append(stmtPtr.*);
    }
    return self.program;
}

fn checkParsingErrors(self: *Parser) void {
    if (self.errors.items.len == 0) return;
    std.debug.print("Parser has {d} error(s).", .{self.errors.items.len});
    for (self.errors.items) |msg| {
        std.debug.print("{s}\n", .{msg});
    }
}

test "test let statements" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();

    const input: []const u8 =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 8331978;
    ;
    const lex = try Lexer.init(testingAlloc, input);
    try lex.tokenize();
    for (lex.tokens.items) |tok| {
        std.debug.print("{s} ", .{tok.Literal});
    }

    const parser = try Parser.init(testingAlloc, lex);

    const program = try parser.parseProgram();
    parser.checkParsingErrors();

    try testing.expectEqual(program.statements.items.len, 3);

    const TestCase = struct {
        expectedIdentifier: []const u8,
    };
    const tests = [_]TestCase{
        .{ .expectedIdentifier = "x" },
        .{ .expectedIdentifier = "y" },
        .{ .expectedIdentifier = "foobar" },
    };

    for (tests, 0..) |t, i| {
        const stmt = program.statements.items[i];
        try testing.expect(std.mem.eql(u8, stmt.tokenLiteral(), "let"));
        try testing.expect(std.mem.eql(u8, stmt.Let.name.value, t.expectedIdentifier));
        try testing.expect(std.mem.eql(u8, stmt.Let.name.tokenLiteral(), t.expectedIdentifier));
    }
}

test "test return statements" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();

    const input: []const u8 =
        \\return 5;
        \\return 10;
        \\return 8331978;
    ;
    const lex = try Lexer.init(testingAlloc, input);
    try lex.tokenize();

    const parser = try Parser.init(testingAlloc, lex);

    const program = try parser.parseProgram();
    parser.checkParsingErrors();

    try testing.expectEqual(program.statements.items.len, 3);

    for (program.statements.items) |stmt| {
        try testing.expect(std.mem.eql(u8, stmt.Return.tokenLiteral(), "return"));
    }
}

test "test integer expressions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const input = "5;";

    const lex = try Lexer.init(testingAlloc, input);
    try lex.tokenize();

    const parser = try Parser.init(testingAlloc, lex);

    const program = try parser.parseProgram();
    parser.checkParsingErrors();

    try testing.expectEqual(1, program.statements.items.len);
    const stmt: AST.ExpressionStatement = program.statements.items[0].Expr;
    const lit = stmt.expression.Integer.value;
    try testing.expectEqual(5, lit);
    try testing.expect(std.mem.eql(u8, "5", stmt.tokenLiteral()));
}

test "test identifier expressions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const input = "foobar;";

    const lex = try Lexer.init(testingAlloc, input);
    try lex.tokenize();

    const parser = try Parser.init(testingAlloc, lex);

    const program = try parser.parseProgram();
    parser.checkParsingErrors();

    try testing.expectEqual(1, program.statements.items.len);
    const stmt: AST.ExpressionStatement = program.statements.items[0].Expr;
    const ident = stmt.expression.Ident;
    try testing.expect(std.mem.eql(u8, "foobar", ident.value));
    try testing.expect(std.mem.eql(u8, "foobar", ident.tokenLiteral()));
}

test "test prefix expressions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const PrefixTest = struct {
        input: []const u8,
        operator: []const u8,
        integerVal: i64,
    };

    const tests = [2]PrefixTest{
        .{ .input = "!5;", .operator = "!", .integerVal = 5 },
        .{ .input = "-15;", .operator = "-", .integerVal = 15 },
    };

    for (tests) |tt| {
        const lex = try Lexer.init(testingAlloc, tt.input);
        try lex.tokenize();

        const parser = try Parser.init(testingAlloc, lex);

        const program = try parser.parseProgram();
        parser.checkParsingErrors();

        try testing.expectEqual(1, program.statements.items.len);
        const stmt: AST.ExpressionStatement = program.statements.items[0].Expr;
        const pref = stmt.expression.Prefix;
        try testing.expectEqualStrings(pref.operator, tt.operator);
    }
}

test "test infix expressions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const InfixTest = struct {
        input: []const u8,
        leftVal: i64,
        operator: []const u8,
        rightVal: i64,
    };

    const tests = [8]InfixTest{
        .{ .input = "10 + 10;", .operator = "+", .rightVal = 10, .leftVal = 10 },
        .{ .input = "12 - 3;", .operator = "-", .rightVal = 3, .leftVal = 12 },
        .{ .input = "18 * 9;", .operator = "*", .rightVal = 9, .leftVal = 18 },
        .{ .input = "12 / 6;", .operator = "/", .rightVal = 6, .leftVal = 12 },
        .{ .input = "12 == 12;", .operator = "==", .rightVal = 12, .leftVal = 12 },
        .{ .input = "12 != 12;", .operator = "!=", .rightVal = 12, .leftVal = 12 },
        .{ .input = "12 > 12;", .operator = ">", .rightVal = 12, .leftVal = 12 },
        .{ .input = "12 < 12;", .operator = "<", .rightVal = 12, .leftVal = 12 },
    };

    for (tests) |tt| {
        const lex = try Lexer.init(testingAlloc, tt.input);
        try lex.tokenize();

        const parser = try Parser.init(testingAlloc, lex);

        const program = try parser.parseProgram();
        parser.checkParsingErrors();

        try testing.expectEqual(1, program.statements.items.len);
        const stmt: AST.ExpressionStatement = program.statements.items[0].Expr;
        const infix = stmt.expression.Infix;
        try testing.expectEqualStrings(infix.operator, tt.operator);
        try testing.expectEqual(infix.left.Integer.value, tt.leftVal);
        try testing.expectEqual(infix.right.Integer.value, tt.rightVal);
    }
}
