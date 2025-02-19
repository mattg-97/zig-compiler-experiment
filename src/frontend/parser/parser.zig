const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer.zig");
const Token = @import("../tokens.zig").Token;
const TokenType = @import("../tokens.zig").TokenType;
const AST = @import("ast.zig");

const Priority = enum(u4) {
    LOWEST = 0,
    EQUALS = 1,
    LESSGREATER = 2,
    SUM = 3,
    PRODUCT = 4,
    PREFIX = 5,
    CALL = 6,
    INDEX = 7,

    fn lessThan(self: Priority, other: Priority) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn fromToken(token: Token) Priority {
        return switch (token.Type) {
            .EQ => .EQUALS,
            .NOT_EQ => .EQUALS,
            .LT => .LESSGREATER,
            .GT => .LESSGREATER,
            .PLUS => .SUM,
            .MINUS => .SUM,
            .SLASH => .PRODUCT,
            .ASTERISK => .PRODUCT,
            .LPAREN => .CALL,
            .LBRACKET => .INDEX,
            else => .LOWEST,
        };
    }
};

pub const ParserError = error{
    ExpectOperator,
    ExpectExpression,
    ExpectIdentifier,
    InvalidPrefix,
    InvalidInfix,
    ExpectPeek,
    ExpectString,
    ExpectArray,
    InvalidHashLiteral,
    InvalidStringLiteral,
    InvalidFunctionParam,
    InvalidBooleanLiteral,
    InvalidIntegerLiteral,
    InvalidInteger,
    InvalidBlockStatement,
    InvalidExpressionList,
    InvalidProgram,
    MemoryAllocation,
};

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token,
    peek_token: Token,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, lexer: *Lexer) ParserError!Self {
        const currentToken = lexer.nextToken();
        const peekToken = lexer.nextToken();
        return .{
            .lexer = lexer,
            .current_token = currentToken,
            .peek_token = peekToken,
            .alloc = alloc,
        };
    }

    fn nextToken(self: *Self) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Self) ParserError!AST.Program {
        var statements = std.ArrayList(AST.Statement).init(self.alloc);
        while (self.current_token.Type != TokenType.EOF) {
            const stmt = try self.parseStatement();
            statements.append(stmt) catch return ParserError.InvalidProgram;
            self.nextToken();
        }
        //self.program.print();
        return AST.Program{ .statements = statements };
    }

    fn parseStatement(self: *Self) ParserError!AST.Statement {
        return switch (self.current_token.Type) {
            TokenType.LET => AST.Statement{ .Let = try self.parseLetStatement() },
            TokenType.RETURN => AST.Statement{ .Return = try self.parseReturnStatement() },
            else => AST.Statement{ .Expr = try self.parseExpressionStatement() },
        };
    }

    fn parseLetStatement(self: *Self) ParserError!AST.LetStatement {
        const letToken = self.current_token;
        try self.expectPeek(.IDENT);

        const name = switch (self.current_token.Type) {
            .IDENT => AST.Identifier{ .token = self.current_token, .value = self.current_token.Literal },
            else => unreachable,
        };
        try self.expectPeek(.ASSIGN);
        self.nextToken();

        const expression = try self.parseExpression(.LOWEST);
        if (self.peek_token.Type == TokenType.SEMICOLON) self.nextToken();

        const expressionPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        expressionPtr.* = expression;
        return AST.LetStatement{ .name = name, .value = expressionPtr, .token = letToken };
    }

    fn parseReturnStatement(self: *Self) ParserError!AST.ReturnStatement {
        const retToken = self.current_token;
        self.nextToken();

        const returnValue = try self.parseExpression(.LOWEST);
        if (self.peek_token.Type == TokenType.SEMICOLON) self.nextToken();

        const returnPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        returnPtr.* = returnValue;
        return AST.ReturnStatement{ .returnValue = returnPtr, .token = retToken };
    }

    fn parseBlockStatement(self: *Self) ParserError!AST.BlockStatement {
        var statements = std.ArrayList(AST.Statement).init(self.alloc);
        const blockToken = self.current_token;

        self.nextToken();

        while (!self.currentTokenIs(.RBRACE) and !self.currentTokenIs(.EOF)) {
            const stmt = try self.parseStatement();
            statements.append(stmt) catch return ParserError.InvalidBlockStatement;
            self.nextToken();
        }
        for (statements.items) |stmt| {
            stmt.print();
        }
        return AST.BlockStatement{ .statements = statements, .token = blockToken };
    }

    fn parseExpressionStatement(self: *Self) ParserError!AST.ExpressionStatement {
        const exprStmtToken = self.current_token;
        const expr = try self.parseExpression(.LOWEST);
        if (self.peek_token.Type == TokenType.SEMICOLON) {
            self.nextToken();
        }
        const exprPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        exprPtr.* = expr;
        return AST.ExpressionStatement{ .expression = exprPtr, .token = exprStmtToken };
    }

    fn parseExpression(self: *Self, precedence: Priority) ParserError!AST.Expression {
        var left = try self.parsePrefixTokens(self.current_token.Type);
        while (!self.peekTokenIs(.SEMICOLON) and precedence.lessThan(Priority.fromToken(self.peek_token))) {
            const leftExprPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
            leftExprPtr.* = left;
            left = try self.parseInfixTokens(self.peek_token.Type, leftExprPtr);
        }
        return left;
    }

    fn peekTokenIs(self: Self, tokenType: TokenType) bool {
        return @intFromEnum(self.peek_token.Type) == @intFromEnum(tokenType);
    }
    fn currentTokenIs(self: Self, tokenType: TokenType) bool {
        return @intFromEnum(self.current_token.Type) == @intFromEnum(tokenType);
    }

    fn parsePrefixTokens(self: *Self, token: TokenType) ParserError!AST.Expression {
        return switch (token) {
            TokenType.INT => AST.Expression{ .Integer = try self.parseIntegerExpression() },
            TokenType.IDENT => AST.Expression{ .Ident = try self.parseIdent() },
            TokenType.MINUS, TokenType.BANG => AST.Expression{ .Prefix = try self.parsePrefix() },
            TokenType.TRUE, TokenType.FALSE => AST.Expression{ .Bool = try self.parseBooleanExpression() },
            TokenType.IF => AST.Expression{ .If = try self.parseIfExpression() },
            TokenType.LPAREN => try self.parseGroupedExpression(),
            TokenType.FUNCTION => AST.Expression{ .Function = try self.parseFunctionLiteral() },
            TokenType.STRING => AST.Expression{ .String = try self.parseStringLiteral() },
            TokenType.LBRACKET => AST.Expression{ .Array = try self.parseArrayLiteral() },
            TokenType.LBRACE => AST.Expression{ .Hash = try self.parseHashLiteral() },
            else => ParserError.InvalidPrefix,
        };
    }

    fn parseInfixTokens(self: *Self, token: TokenType, left: *AST.Expression) ParserError!AST.Expression {
        self.nextToken();
        return switch (token) {
            .PLUS, .ASTERISK, .SLASH, .GT, .LT, .EQ, .NOT_EQ, .MINUS, .ASSIGN => AST.Expression{ .Infix = try self.parseInfix(left) },
            .LPAREN => AST.Expression{ .Call = try self.parseCallExpression(left) },
            .LBRACKET => AST.Expression{ .Index = try self.parseIndexExpression(left) },
            else => ParserError.InvalidInfix,
        };
    }

    fn parseIdent(self: *Self) ParserError!AST.Identifier {
        if (self.current_token.Type != TokenType.IDENT) return ParserError.ExpectIdentifier;
        return AST.Identifier{ .token = self.current_token, .value = self.current_token.Literal };
    }

    fn parseIntegerExpression(self: *Self) ParserError!AST.IntegerExpression {
        if (self.current_token.Type != .INT) return ParserError.InvalidIntegerLiteral;
        const intVal = std.fmt.parseInt(i64, self.current_token.Literal, 10) catch return ParserError.InvalidInteger;
        return AST.IntegerExpression{ .token = self.current_token, .value = intVal };
    }

    fn parseBooleanExpression(self: *Self) ParserError!AST.BooleanExpression {
        return switch (self.current_token.Type) {
            .TRUE => AST.BooleanExpression{ .token = self.current_token, .value = true },
            .FALSE => AST.BooleanExpression{ .token = self.current_token, .value = false },
            else => ParserError.InvalidBooleanLiteral,
        };
    }

    fn parseGroupedExpression(self: *Self) ParserError!AST.Expression {
        self.nextToken();
        const expr = try self.parseExpression(.LOWEST);
        try self.expectPeek(.RPAREN);
        return expr;
    }

    fn parsePrefix(self: *Self) ParserError!AST.PrefixExpression {
        const op = AST.PrefixOperator.fromString(self.current_token.Literal);
        self.nextToken();
        const right = try self.parseExpression(.PREFIX);
        const rightPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        rightPtr.* = right;
        return AST.PrefixExpression{ .token = self.current_token, .operator = op, .right = rightPtr };
    }

    fn parseInfix(self: *Self, left: *AST.Expression) ParserError!AST.InfixExpression {
        const infixToken = self.current_token;
        const op = AST.InfixOperator.fromString(self.current_token.Literal);
        const prec = Priority.fromToken(self.current_token);

        self.nextToken();

        const right = try self.parseExpression(prec);
        const exprPointer = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        exprPointer.* = right;
        return AST.InfixExpression{ .token = infixToken, .left = left, .operator = op, .right = exprPointer };
    }

    fn parseIfExpression(self: *Self) ParserError!AST.IfExpression {
        try self.expectPeek(.LPAREN);

        self.nextToken();

        const condition = try self.parseExpression(.LOWEST);
        const conditionPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        conditionPtr.* = condition;

        try self.expectPeek(.RPAREN);
        try self.expectPeek(.LBRACE);

        const consequenceBlock = try self.parseBlockStatement();
        var alternativeBlock: ?AST.BlockStatement = null;

        if (self.peek_token.Type == .ELSE) {
            self.nextToken();
            try self.expectPeek(.LBRACE);

            alternativeBlock = try self.parseBlockStatement();
        }
        return AST.IfExpression{ .condition = conditionPtr, .consequence = consequenceBlock, .alternative = alternativeBlock, .token = self.current_token };
    }

    fn parseFunctionLiteral(self: *Self) ParserError!AST.FunctionLiteral {
        try self.expectPeek(.LPAREN);

        const params = try self.parseFunctionParameters();
        try self.expectPeek(.LBRACE);

        const body = try self.parseBlockStatement();
        const bodyPtr = self.alloc.create(AST.BlockStatement) catch return ParserError.MemoryAllocation;
        bodyPtr.* = body;
        return AST.FunctionLiteral{ .token = self.current_token, .body = bodyPtr, .parameters = params };
    }

    fn parseFunctionParameters(self: *Self) ParserError!std.ArrayList(AST.Identifier) {
        var params = std.ArrayList(AST.Identifier).init(self.alloc);
        if (self.peekTokenIs(.RPAREN)) {
            self.nextToken();
            return params;
        }
        self.nextToken();

        params.append(try self.parseIdent()) catch return ParserError.InvalidFunctionParam;

        while (self.peekTokenIs(.COMMA)) {
            self.nextToken();
            self.nextToken();
            params.append(try self.parseIdent()) catch return ParserError.InvalidFunctionParam;
        }
        try self.expectPeek(.RPAREN);
        return params;
    }

    fn parseCallExpression(self: *Parser, func: *AST.Expression) ParserError!AST.CallExpression {
        return AST.CallExpression{ .token = self.current_token, .function = func, .args = try self.parseExpressions(.RPAREN) };
    }

    fn parseExpressions(self: *Parser, closeToken: TokenType) ParserError!std.ArrayList(AST.Expression) {
        var exprList = std.ArrayList(AST.Expression).init(self.alloc);
        if (self.peekTokenIs(closeToken)) {
            self.nextToken();
            return exprList;
        }

        self.nextToken();
        exprList.append(try self.parseExpression(Priority.LOWEST)) catch return ParserError.InvalidExpressionList;

        while (self.peek_token.Type == TokenType.COMMA) {
            self.nextToken();
            self.nextToken();
            exprList.append(try self.parseExpression(Priority.LOWEST)) catch return ParserError.InvalidExpressionList;
        }
        try self.expectPeek(closeToken);
        return exprList;
    }
    fn parseStringLiteral(self: *Parser) ParserError!AST.StringLiteral {
        if (!self.currentTokenIs(.STRING)) return ParserError.ExpectString;
        return AST.StringLiteral{ .token = self.current_token, .value = self.current_token.Literal };
    }

    fn parseArrayLiteral(self: *Self) ParserError!AST.ArrayLiteral {
        if (!self.currentTokenIs(.LBRACKET)) return ParserError.ExpectArray;
        return AST.ArrayLiteral{ .token = self.current_token, .elements = try self.parseExpressions(TokenType.RBRACKET) };
    }

    fn parseIndexExpression(self: *Self, left: *AST.Expression) ParserError!AST.IndexExpression {
        if (!self.currentTokenIs(.LBRACKET)) return ParserError.ExpectArray;
        const indexToken = self.current_token;
        self.nextToken();
        const index = try self.parseExpression(.LOWEST);
        const indexPtr = self.alloc.create(AST.Expression) catch return ParserError.MemoryAllocation;
        indexPtr.* = index;

        try self.expectPeek(.RBRACKET);
        return .{
            .index = indexPtr,
            .left = left,
            .token = indexToken,
        };
    }

    fn parseHashLiteral(self: *Self) ParserError!AST.HashLiteral {
        var hash = AST.HashLiteral{ .token = self.current_token, .pairs = std.ArrayList(AST.HashPair).init(self.alloc) };
        while (!self.peekTokenIs(.RBRACE)) {
            self.nextToken();
            const key = try self.parseExpression(.LOWEST);
            try self.expectPeek(.COLON);
            self.nextToken();
            const value = try self.parseExpression(.LOWEST);
            hash.pairs.append(AST.HashPair{ .token = self.current_token, .key = key, .value = value }) catch return ParserError.InvalidHashLiteral;
            if (!self.peekTokenIs(.RBRACE)) {
                self.expectPeek(.COMMA) catch return ParserError.InvalidHashLiteral;
            }
        }
        try self.expectPeek(.RBRACE);
        return hash;
    }

    fn peekError(self: *Parser, t: TokenType) !void {
        const msg = try std.fmt.allocPrint(self.alloc, "Expected token to be {s} but got {s} instead.\n", .{ std.enums.tagName(TokenType, t).?, std.enums.tagName(TokenType, self.peek_token.Type).? });
        std.debug.print("ERROR: {s}\n", .{msg});
        try self.errors.append(msg);
    }

    fn expectPeek(self: *Self, tokenType: TokenType) ParserError!void {
        if (self.peek_token.Type == tokenType) {
            self.nextToken();
        } else {
            return ParserError.ExpectPeek;
        }
    }

    fn peekPrecedence(self: *Parser) Priority {
        return AST.getTokenPrecedence(&self.peek_token);
    }

    fn currentPrecedence(self: *Parser) Priority {
        return AST.getTokenPrecedence(&self.current_token);
    }

    fn parsePrintStatement(self: *Parser) !*AST.Statement {
        self.nextToken();
        const printStmt: AST.PrintStatement = .{ .token = self.current_token, .value = try self.parseExpression(Priority.LOWEST) };
        const stmt: AST.Statement = .{ .Print = printStmt };
        while (self.current_token.Type != TokenType.SEMICOLON) : (self.nextToken()) {}
        const stmtPointer = try self.alloc.create(AST.Statement);
        stmtPointer.* = stmt;
        return stmtPointer;
    }
};
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

        try testing.expectEqual(1, program.statements.items.len);
        const stmt: AST.ExpressionStatement = program.statements.items[0].Expr;
        const infix = stmt.expression.Infix;
        std.debug.assert(infix.operator == tt.operator);
        try testing.expectEqual(infix.left.Integer.value, tt.leftVal);
        try testing.expectEqual(infix.right.Integer.value, tt.rightVal);
    }
}
