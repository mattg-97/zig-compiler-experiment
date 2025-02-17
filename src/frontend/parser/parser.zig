const Parser = @This();

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer.zig");
const Token = @import("../tokens.zig").Token;
const TokenType = @import("../tokens.zig").TokenType;
const AST = @import("ast.zig");
const BoxValue = @import("../../utils/memory.zig").BoxValue;

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

fn parseCallExpression(self: *Parser) !*AST.Expression {
    const expr = try self.alloc.create(AST.Expression);
    expr.*.Call = .{
        .args = try self.parseCallArguments(),
        .token = self.current_token,
        .function = undefined,
    };
    return expr;
}

fn parseCallArguments(self: *Parser) ![]*AST.Expression {
    const args: std.ArrayList(*AST.Expression) = try std.ArrayList(*AST.Expression).init(self.alloc);
    if (self.peek_token.Type == TokenType.RPAREN) {
        self.nextToken();
        return args.items;
    }
    self.nextToken();
    try args.append(try self.parseExpression(AST.Precedence.LOWEST));

    while (self.peek_token.Type == TokenType.COMMA) {
        self.nextToken();
        self.nextToken();
        try args.append(try self.parseExpression(AST.Precedence.LOWEST));
    }
    try self.expectPeek(TokenType.RPAREN);
    return args.items;
}

fn parseFunctionLiteral(self: *Parser) !*AST.Expression {
    const lit = try self.alloc.create(AST.Expression);
    lit.* = .{
        .Function = .{
            .token = self.current_token,
            .body = undefined,
            .parameters = try std.ArrayList(*AST.Identifier).init(self.alloc),
        },
    };
    try self.expectPeek(TokenType.LPAREN);
    lit.*.Function.parameters = try self.parseFunctionParameters();
    try self.expectPeek(TokenType.LBRACE);
    lit.*.Function.body = try self.parseBlockStatement();
    return lit;
}

fn parseFunctionParameters(self: *Parser) ![]*AST.Identifier {
    const idents: std.ArrayList(*AST.Identifier) = try std.ArrayList(*AST.Identifier).init(self.alloc);
    if (self.peek_token.Type == TokenType.RPAREN) {
        self.nextToken();
        return idents.items;
    }
    self.nextToken();
    const ident: AST.Identifier = try self.alloc.create(AST.Identifier);
    ident.* = .{
        .token = self.current_token,
        .value = self.current_token.Literal,
    };
    try idents.append(ident);
    while (self.peek_token.Type == TokenType.COMMA) {
        self.nextToken();
        self.nextToken();
        const identInner: AST.Identifier = try self.alloc.create(AST.Identifier);
        identInner.* = .{
            .token = self.current_token,
            .value = self.current_token.Literal,
        };
        try idents.append(ident);
    }
    try self.expectPeek(TokenType.RPAREN);
    return idents.items;
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

fn peekPrecedence(self: *Parser) AST.Precedence {
    return AST.getTokenPrecedence(&self.peek_token);
}

fn currentPrecedence(self: *Parser) AST.Precedence {
    return AST.getTokenPrecedence(&self.current_token);
}

fn parseLetStatement(self: *Parser) !*AST.Statement {
    var letStmt: AST.LetStatement = .{
        .name = undefined,
        .value = undefined,
        .token = self.current_token,
    };
    try self.expectPeek(TokenType.IDENT);
    letStmt.name = .{ .token = self.current_token, .value = self.current_token.Literal };
    try self.expectPeek(TokenType.ASSIGN);
    self.nextToken();
    letStmt.value = try self.parseExpression(AST.Precedence.LOWEST);
    if (self.peek_token.Type == TokenType.SEMICOLON) self.nextToken();
    const stmt: AST.Statement = .{ .Let = letStmt };
    return BoxValue(AST.Statement, stmt, &self.alloc);
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

fn parseGroupedExpression(self: *Parser) !*AST.Expression {
    self.nextToken();
    const expr = try self.parseExpression(AST.Precedence.LOWEST);
    try self.expectPeek(TokenType.RPAREN);
    return expr;
}

fn parseReturnStatement(self: *Parser) !*AST.Statement {
    var retStmt: AST.ReturnStatement = .{
        .returnValue = undefined,
        .token = self.current_token,
    };
    self.nextToken();
    retStmt.returnValue = try self.parseExpression(AST.Precedence.LOWEST);
    if (self.peek_token.Type == TokenType.SEMICOLON) self.nextToken();
    const stmt: AST.Statement = .{ .Return = retStmt };
    return BoxValue(AST.Statement, stmt, &self.alloc);
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
    const prefix: AST.PrefixExpression = .{ .token = prefixToken, .operator = AST.PrefixOperator.fromString(prefixToken.Literal), .right = try self.parseExpression(AST.Precedence.PREFIX) };
    const expr: AST.Expression = .{ .Prefix = prefix };
    const exprPointer = try self.alloc.create(AST.Expression);
    exprPointer.* = expr;
    return exprPointer;
}

fn parseBooleanExpression(self: *Parser) !*AST.Expression {
    const expr = try self.alloc.create(AST.Expression);
    const boolExpr: AST.BooleanExpression = .{ .token = self.current_token, .value = (self.current_token.Type == TokenType.TRUE) };
    expr.* = .{ .Bool = boolExpr };
    return expr;
}

fn parseBlockStatement(self: *Parser) !*AST.Statement {
    const block = try self.alloc.create(AST.Statement);
    block.* = .{
        .Block = .{
            .token = self.current_token,
            .statements = std.ArrayList(*AST.Statement).init(self.alloc),
        },
    };

    self.nextToken();
    if (self.current_token.Type == TokenType.LBRACE) self.nextToken();

    while (self.current_token.Type != TokenType.RBRACE and self.current_token.Type != TokenType.EOF) {
        const stmt = try self.parseStatement();
        try block.*.Block.statements.append(stmt);
        if (self.peek_token.Type == TokenType.EOF) self.nextToken();
        self.nextToken();
    }
    return block;
}

fn parseIfExpression(self: *Parser) !*AST.Expression {
    const expr = try self.alloc.create(AST.Expression);
    expr.* = .{
        .If = .{
            .token = self.current_token,
            .alternative = undefined,
            .consequence = undefined,
            .condition = undefined,
        },
    };
    try self.expectPeek(TokenType.LPAREN);
    self.nextToken();
    expr.*.If.condition = try self.parseExpression(AST.Precedence.LOWEST);
    try self.expectPeek(TokenType.RPAREN);
    try self.expectPeek(TokenType.LBRACE);
    expr.*.If.consequence = try self.parseBlockStatement();
    if (self.peek_token.Type == TokenType.ELSE) {
        self.nextToken();
        try self.expectPeek(TokenType.LBRACE);
        expr.*.If.alternative.? = try self.parseBlockStatement();
    } else {
        expr.*.If.alternative = null;
    }
    return expr;
}

fn parseInfix(self: *Parser, left: *AST.Expression) !*AST.Expression {
    const infixToken = self.current_token;
    const prec = AST.getTokenPrecedence(&self.current_token);
    self.nextToken();
    const right = try self.parseExpression(prec);
    const infix: AST.InfixExpression = .{ .token = infixToken, .operator = AST.InfixOperator.fromString(infixToken.Literal), .left = left, .right = right };
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
        TokenType.TRUE, TokenType.FALSE => left = try self.parseBooleanExpression(),
        TokenType.IF => left = try self.parseIfExpression(),
        TokenType.LPAREN => left = try self.parseGroupedExpression(),
        else => std.debug.print("TOKEN: {s} type: {s}\n", .{ self.current_token.Literal, std.enums.tagName(TokenType, self.current_token.Type).? }),
    }
    while ((self.peek_token.Type != TokenType.SEMICOLON or self.peek_token.Type != TokenType.EOF) and @intFromEnum(precedence) <= @intFromEnum(AST.getTokenPrecedence(&self.peek_token))) {
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

fn parseStatement(self: *Parser) anyerror!*AST.Statement {
    return switch (self.current_token.Type) {
        TokenType.LET => try self.parseLetStatement(),
        TokenType.RETURN => try self.parseReturnStatement(),
        TokenType.PRINT => try self.parsePrintStatement(),
        TokenType.LBRACE => try self.parseBlockStatement(),
        else => try self.parseExpressionStatement(),
    };
}

pub fn parseProgram(self: *Parser) !*AST.Program {
    while (self.current_token.Type != TokenType.EOF) {
        const stmtPtr = try self.parseStatement();
        self.nextToken();
        try self.program.statements.append(stmtPtr.*);
    }
    self.program.print();
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
        operator: AST.PrefixOperator,
        integerVal: i64,
    };

    const tests = [2]PrefixTest{
        .{ .input = "!5;", .operator = AST.PrefixOperator.BANG, .integerVal = 5 },
        .{ .input = "-15;", .operator = AST.PrefixOperator.MINUS, .integerVal = 15 },
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
        std.debug.assert(pref.operator == tt.operator);
    }
}

test "test infix expressions" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();
    const InfixTest = struct {
        input: []const u8,
        leftVal: i64,
        operator: AST.InfixOperator,
        rightVal: i64,
    };

    const tests = [8]InfixTest{
        .{ .input = "10 + 10;", .operator = AST.InfixOperator.PLUS, .rightVal = 10, .leftVal = 10 },
        .{ .input = "12 - 3;", .operator = AST.InfixOperator.MINUS, .rightVal = 3, .leftVal = 12 },
        .{ .input = "18 * 9;", .operator = AST.InfixOperator.MULTIPLY, .rightVal = 9, .leftVal = 18 },
        .{ .input = "12 / 6;", .operator = AST.InfixOperator.DIVIDE, .rightVal = 6, .leftVal = 12 },
        .{ .input = "12 == 12;", .operator = AST.InfixOperator.EQUAL, .rightVal = 12, .leftVal = 12 },
        .{ .input = "12 != 12;", .operator = AST.InfixOperator.NOT_EQUAL, .rightVal = 12, .leftVal = 12 },
        .{ .input = "12 > 12;", .operator = AST.InfixOperator.GREATER_THAN, .rightVal = 12, .leftVal = 12 },
        .{ .input = "12 < 12;", .operator = AST.InfixOperator.LESS_THAN, .rightVal = 12, .leftVal = 12 },
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
        std.debug.assert(infix.operator == tt.operator);
        try testing.expectEqual(infix.left.Integer.value, tt.leftVal);
        try testing.expectEqual(infix.right.Integer.value, tt.rightVal);
    }
}
