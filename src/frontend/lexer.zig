const Lexer = @This();

const std = @import("std");
const testing = std.testing;

const tokens = @import("tokens.zig");
const token = tokens.Token;
const tokenType = tokens.TokenType;

alloc: std.mem.Allocator,
input: []const u8,
position: u32,
readPosition: u32,
ch: u8,
line: usize,
const log = std.log.scoped(.lexer);

pub fn init(alloc: std.mem.Allocator, input: []const u8) !*Lexer {
    var l = try alloc.create(Lexer);
    l.* = Lexer{
        .alloc = alloc,
        .input = input,
        .position = 0,
        .readPosition = 0,
        .ch = 0,
        .line = 0,
    };

    l.readChar();
    return l;
}

fn tokenize(self: *Lexer) !void {
    var tok = tokens.Token{
        .Literal = "",
        .Type = tokens.TokenType.ILLEGAL,
        .Length = 0,
        .Line = self.line,
    };
    while (tok.Type != tokens.TokenType.EOF) {
        tok = try self.nextToken();
    }
    log.info("Lexing completed.\n", .{});
}

fn readIdentifier(self: *Lexer) []const u8 {
    const position = self.position;
    while (isLetter(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

pub fn nextToken(self: *Lexer) token {
    var tok: token = token{
        .Literal = "",
        .Type = tokenType.ILLEGAL,
        .Line = self.line,
        .Length = 0,
    };
    self.skipWhitespace();
    switch (self.ch) {
        ';' => tok = token.newToken(tokenType.SEMICOLON, null, self.line),
        '=' => {
            if (self.peekChar() == '=') {
                tok = token.newToken(tokenType.EQ, null, self.line);
                self.readChar();
            } else {
                tok = token.newToken(tokenType.ASSIGN, null, self.line);
            }
        },
        '(' => tok = token.newToken(tokenType.LPAREN, null, self.line),
        ')' => tok = token.newToken(tokenType.RPAREN, null, self.line),
        '{' => tok = token.newToken(tokenType.LBRACE, null, self.line),
        '}' => tok = token.newToken(tokenType.RBRACE, null, self.line),
        ',' => tok = token.newToken(tokenType.COMMA, null, self.line),
        '+' => tok = token.newToken(tokenType.PLUS, null, self.line),
        '-' => tok = token.newToken(tokenType.MINUS, null, self.line),
        '!' => {
            if (self.peekChar() == '=') {
                tok = token.newToken(tokenType.NOT_EQ, null, self.line);
                self.readChar();
            } else {
                tok = token.newToken(tokenType.BANG, null, self.line);
            }
        },
        '/' => tok = token.newToken(tokenType.SLASH, null, self.line),
        '*' => tok = token.newToken(tokenType.ASTERISK, null, self.line),
        '>' => tok = token.newToken(tokenType.GT, null, self.line),
        '<' => tok = token.newToken(tokenType.LT, null, self.line),
        '"' => {
            tok.Type = tokenType.STRING;
            tok.Literal = self.readString();
        },
        '[' => tok = token.newToken(tokenType.LBRACKET, null, self.line),
        ']' => tok = token.newToken(tokenType.RBRACKET, null, self.line),
        '\x00' => tok = token.newToken(tokenType.EOF, null, self.line),
        else => {
            if (isLetter(self.ch)) {
                tok.Literal = self.readIdentifier();
                tok.Type = tokens.lookupIdent(tok.Literal);
                tok.Line = self.line;
                log.debug("Token created: {s} of type {s}\n", .{ tok.Literal, tok.Type.toTokenLiteral() });
                return tok;
            } else if (isDigit(self.ch)) {
                tok.Type = tokenType.INT;
                tok.Literal = self.readNumber();
                tok.Line = self.line;
                log.debug("Token created: {s} of type {s}\n", .{ tok.Literal, tok.Type.toTokenLiteral() });
                return tok;
            } else {
                tok = token.newToken(tokenType.ILLEGAL, null, self.line);
            }
        },
    }
    self.readChar();
    log.debug("Token created: {s} of type {s}\n", .{ tok.Literal, tok.Type.toTokenLiteral() });
    return tok;
}

fn readChar(self: *Lexer) void {
    if (self.readPosition >= self.input.len) {
        self.ch = 0;
    } else {
        self.ch = self.input[self.readPosition];
    }
    self.position = self.readPosition;
    self.readPosition += 1;
}

fn readString(self: *Lexer) []const u8 {
    const position = self.position + 1;
    while (true) {
        self.readChar();
        if (self.ch == '"' or self.ch == 0) break;
    }
    return self.input[position..self.position];
}

fn readNumber(self: *Lexer) []const u8 {
    const position = self.position;
    while (isDigit(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn skipWhitespace(self: *Lexer) void {
    while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
        if (self.ch == '\n') self.line += 1;
        self.readChar();
    }
}

fn peekChar(self: *Lexer) u8 {
    if (self.readPosition >= self.input.len) {
        return 0;
    } else {
        return self.input[self.readPosition];
    }
}

fn isLetter(ch: u8) bool {
    return (std.ascii.isAlphabetic(ch) or ch == '_');
}

fn isDigit(ch: u8) bool {
    return std.ascii.isDigit(ch);
}

const TestToken = struct { expectedType: tokenType, expectedLiteral: []const u8 };

test "test lexer" {
    var testingArena = std.heap.ArenaAllocator.init(testing.allocator);
    defer testingArena.deinit();
    const testingAlloc = testingArena.allocator();

    const input: []const u8 =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
        \\
    ;
    const testTokens = [_]TestToken{
        .{ .expectedType = tokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "five" },
        .{ .expectedType = tokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = tokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "add" },
        .{ .expectedType = tokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = tokenType.FUNCTION, .expectedLiteral = "fn" },
        .{ .expectedType = tokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "x" },
        .{ .expectedType = tokenType.COMMA, .expectedLiteral = "," },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "y" },
        .{ .expectedType = tokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = tokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "x" },
        .{ .expectedType = tokenType.PLUS, .expectedLiteral = "+" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "y" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.LET, .expectedLiteral = "let" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "result" },
        .{ .expectedType = tokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "add" },
        .{ .expectedType = tokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "five" },
        .{ .expectedType = tokenType.COMMA, .expectedLiteral = "," },
        .{ .expectedType = tokenType.IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = tokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.BANG, .expectedLiteral = "!" },
        .{ .expectedType = tokenType.MINUS, .expectedLiteral = "-" },
        .{ .expectedType = tokenType.SLASH, .expectedLiteral = "/" },
        .{ .expectedType = tokenType.ASTERISK, .expectedLiteral = "*" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = tokenType.LT, .expectedLiteral = "<" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = tokenType.GT, .expectedLiteral = ">" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.IF, .expectedLiteral = "if" },
        .{ .expectedType = tokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "5" },
        .{ .expectedType = tokenType.LT, .expectedLiteral = "<" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = tokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = tokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = tokenType.RETURN, .expectedLiteral = "return" },
        .{ .expectedType = tokenType.TRUE, .expectedLiteral = "true" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = tokenType.ELSE, .expectedLiteral = "else" },
        .{ .expectedType = tokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = tokenType.RETURN, .expectedLiteral = "return" },
        .{ .expectedType = tokenType.FALSE, .expectedLiteral = "false" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = tokenType.EQ, .expectedLiteral = "==" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "10" },
        .{ .expectedType = tokenType.NOT_EQ, .expectedLiteral = "!=" },
        .{ .expectedType = tokenType.INT, .expectedLiteral = "9" },
        .{ .expectedType = tokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = tokenType.EOF, .expectedLiteral = "\x00" },
    };

    const lex = try Lexer.init(testingAlloc, input);

    for (testTokens) |testToken| {
        const tok = try lex.nextToken();
        try testing.expectEqual(testToken.expectedType, tok.Type);
        try testing.expect(std.mem.eql(u8, testToken.expectedLiteral, tok.Literal));
    }
}
