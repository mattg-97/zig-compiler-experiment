pub const Token = @This();

const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    EQ,
    NOT_EQ,
    ERROR,
    PRINT,
    pub fn toTokenLiteral(self: TokenType) []const u8 {
        const tokenStr = switch (self) {
            TokenType.EOF => "\x00",
            TokenType.IDENT => "IDENT",
            TokenType.INT => "INT",
            TokenType.ASSIGN => "=",
            TokenType.PLUS => "+",
            TokenType.COMMA => ",",
            TokenType.SEMICOLON => ";",
            TokenType.LPAREN => "(",
            TokenType.RPAREN => ")",
            TokenType.LBRACE => "{",
            TokenType.RBRACE => "}",
            TokenType.FUNCTION => "FUNCTION",
            TokenType.LET => "LET",
            TokenType.MINUS => "-",
            TokenType.BANG => "!",
            TokenType.ASTERISK => "*",
            TokenType.SLASH => "/",
            TokenType.LT => "<",
            TokenType.GT => ">",
            TokenType.TRUE => "TRUE",
            TokenType.FALSE => "FALSE",
            TokenType.IF => "IF",
            TokenType.ELSE => "ELSE",
            TokenType.RETURN => "RETURN",
            TokenType.EQ => "==",
            TokenType.NOT_EQ => "!=",
            TokenType.ERROR => "ERROR",
            TokenType.PRINT => "PRINT",
            else => "ILLEGAL",
        };
        return tokenStr;
    }
};

Type: TokenType,
Literal: []const u8,
Line: usize,
Length: usize,
const Self = @This();
pub fn newToken(tokenType: TokenType, lit: ?[]const u8, line: usize) !Self {
    if (lit == null) {
        return Self{
            .Type = tokenType,
            .Literal = tokenType.toTokenLiteral(),
            .Line = line,
            .Length = tokenType.toTokenLiteral().len,
        };
    } else {
        return Self{
            .Type = tokenType,
            .Literal = lit.?,
            .Line = line,
            .Length = lit.?.len,
        };
    }
}

const Keyword = struct { Key: []const u8, Value: TokenType };
const map = [_]Keyword{
    .{ .Key = "fn", .Value = TokenType.FUNCTION },
    .{ .Key = "let", .Value = TokenType.LET },
    .{ .Key = "true", .Value = TokenType.TRUE },
    .{ .Key = "false", .Value = TokenType.FALSE },
    .{ .Key = "if", .Value = TokenType.IF },
    .{ .Key = "else", .Value = TokenType.ELSE },
    .{ .Key = "return", .Value = TokenType.RETURN },
    .{ .Key = "print", .Value = TokenType.PRINT },
};

pub fn lookupIdent(key: []const u8) TokenType {
    for (map) |entry| {
        if (std.mem.eql(u8, entry.Key, key)) {
            return entry.Value;
        }
    }
    return TokenType.IDENT;
}
