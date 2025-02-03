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
const Parser = @This();
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
    try p.nextToken();
    try p.nextToken();
    return p;
}

pub fn destroy(self: *Parser) void {
    for (self.errors.items) |itm| {
        self.alloc.free(itm);
    }
    self.errors.deinit();
    self.alloc.destroy(self);
}

fn nextToken(self: *Parser) !void {
    self.current_token = self.peek_token;
    if (self.peek_token.Type != TokenType.EOF) {
        self.peek_token = self.lexer.tokens.items[self.current];
    }
    self.current += 1;
}

fn expectPeek(self: *Parser, tokenType: TokenType) !void {
    if (self.peek_token.Type == tokenType) {
        try self.nextToken();
    } else {
        try self.peekError(tokenType);
    }
}

fn parseLetStatement(self: *Parser) !void {
    try self.expectPeek(TokenType.IDENT);
    try self.expectPeek(TokenType.ASSIGN);
    const letIdent: AST.Identifier = .{ .token = self.current_token, .value = self.current_token.Literal };
    const letStmt: AST.LetStatement = .{ .token = self.current_token, .name = letIdent, .value = null };
    const stmt: AST.Statement = .{ .Let = letStmt };
    while (self.current_token.Type != TokenType.SEMICOLON) : (try self.nextToken()) {}
    try self.program.statements.append(stmt);
}

fn parseReturnStatement(self: *Parser) !void {
    const returnStmt: AST.ReturnStatement = .{ .token = self.current_token, .returnValue = null };
    const stmt: AST.Statement = .{ .Return = returnStmt };
    try self.nextToken();
    while (self.current_token.Type != TokenType.SEMICOLON) : (try self.nextToken()) {}
    try self.program.statements.append(stmt);
}

fn parseIntegerExpression(self: *Parser) !AST.Expression {
    const intVal = try std.fmt.parseInt(i64, self.current_token.Literal, 10);
    const intExpr: AST.IntegerExpression = .{ .token = self.current_token, .value = intVal };
    const expr: AST.Expression = .{ .Integer = intExpr };
    return expr;
}

fn parseExpression(self: *Parser, _: AST.Precedence) !AST.Expression {
    var left: AST.Expression = undefined;
    switch (self.current_token.Type) {
        TokenType.INT => left = try self.parseIntegerExpression(),
        else => @panic("unable to parse this expression type yet"),
    }
    return left;
}

fn parseExpressionStatement(self: *Parser) !void {
    const exprStmt: AST.ExpressionStatement = .{ .token = self.current_token, .expression = try self.parseExpression(AST.Precedence.LOWEST) };
    const stmt: AST.Statement = .{ .Expr = exprStmt };
    while (self.current_token.Type != TokenType.SEMICOLON) : (try self.nextToken()) {}
    try self.program.statements.append(stmt);
}

fn parseStatement(self: *Parser) !void {
    switch (self.current_token.Type) {
        TokenType.LET => try self.parseLetStatement(),
        TokenType.RETURN => try self.parseReturnStatement(),
        else => try self.parseExpressionStatement(),
    }
}

pub fn parseProgram(self: *Parser) !*AST.Program {
    while (self.current_token.Type != TokenType.EOF) {
        try self.parseStatement();
        try self.nextToken();
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
    const testingAlloc = testing.allocator;

    const input: []const u8 =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 8331978;
    ;
    const lex = try Lexer.init(testingAlloc, input);
    defer lex.destroy();
    try lex.tokenize();
    for (lex.tokens.items) |tok| {
        std.debug.print("{s} ", .{tok.Literal});
    }

    const parser = try Parser.init(testingAlloc, lex);
    defer parser.destroy();

    const program = try parser.parseProgram();
    defer program.destroy();
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
    const testingAlloc = testing.allocator;

    const input: []const u8 =
        \\return 5;
        \\return 10;
        \\return 8331978;
    ;
    const lex = try Lexer.init(testingAlloc, input);
    defer lex.destroy();
    try lex.tokenize();
    for (lex.tokens.items) |tok| {
        std.debug.print("{s} ", .{tok.Literal});
    }

    const parser = try Parser.init(testingAlloc, lex);
    defer parser.destroy();

    const program = try parser.parseProgram();
    defer program.destroy();
    parser.checkParsingErrors();

    try testing.expectEqual(program.statements.items.len, 3);

    for (program.statements.items) |stmt| {
        try testing.expect(std.mem.eql(u8, stmt.Return.tokenLiteral(), "return"));
    }
}
