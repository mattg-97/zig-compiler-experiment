const AST = @This();

const std = @import("std");

const Token = @import("../tokens.zig").Token;
const TokenType = @import("../tokens.zig").TokenType;

pub const Precedence = enum(u8) {
    LOWEST,
    LOGICAL_OR,
    LOGICAL_AND,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
};

pub fn getTokenPrecedence(token: *Token) Precedence {
    switch (token.Type) {
        TokenType.ASSIGN => return Precedence.EQUALS,
        TokenType.EQ => return Precedence.EQUALS,
        TokenType.NOT_EQ => return Precedence.EQUALS,
        TokenType.LT => return Precedence.LESSGREATER,
        TokenType.GT => return Precedence.LESSGREATER,
        TokenType.PLUS => return Precedence.SUM,
        TokenType.MINUS => return Precedence.SUM,
        TokenType.SLASH => return Precedence.PRODUCT,
        TokenType.ASTERISK => return Precedence.PRODUCT,
        TokenType.LPAREN => return Precedence.CALL,
        TokenType.LBRACE => return Precedence.INDEX,
        else => return Precedence.LOWEST,
    }
}

pub const Statement = union(enum) {
    Let: LetStatement,
    Return: ReturnStatement,
    Expr: ExpressionStatement,
    Print: PrintStatement,
    //Other statement variants
    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            .Let => |stmt| stmt.tokenLiteral(),
            .Return => |stmt| stmt.tokenLiteral(),
            .Expr => |stmt| stmt.tokenLiteral(),
            .Print => |stmt| stmt.tokenLiteral(),
        };
    }
};

pub const IntegerExpression = struct {
    value: i64,
    token: Token,
    pub fn tokenLiteral(self: IntegerExpression) []const u8 {
        return self.token.Literal;
    }
};

pub const Expression = union(enum) {
    Integer: IntegerExpression,
    Ident: Identifier,
    Prefix: PrefixExpression,
    Infix: InfixExpression,
    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            IntegerExpression => |expr| expr.tokenLiteral(),
            Identifier => |expr| expr.tokenLiteral(),
            PrefixExpression => |expr| expr.tokenLiteral(),
            InfixExpression => |expr| expr.tokenLiteral(),
        };
    }
};

pub const PrintStatement = struct {
    token: Token,
    value: *Expression,
    pub fn tokenLiteral(self: PrintStatement) []const u8 {
        return self.token.Literal;
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: *Expression,
    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.Literal;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,
    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.Literal;
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,
    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.Literal;
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression,
    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.Literal;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    returnValue: *Expression,
    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.Literal;
    }
};
pub const Identifier = struct {
    token: Token,
    value: []const u8,
    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.Literal;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    alloc: std.mem.Allocator,
    const Self = @This();
    pub fn tokenLiteral(self: Self) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn init(alloc: std.mem.Allocator) !*Self {
        const p = try alloc.create(Self);
        p.* = Self{
            .statements = std.ArrayList(Statement).init(alloc),
            .alloc = alloc,
        };
        return p;
    }

    //pub fn destroy(self: *Self) void {
    // for (self.statements.items) |stmt| {
    //     destroyStatement(self.alloc, @constCast(&stmt));
    // }
    //  self.statements.deinit();
    //self.alloc.destroy(self);
    //}
};

//fn destroyExpression(alloc: std.mem.Allocator, ptr: *Expression) void {
//    switch (ptr.*) {
//        .Infix => |e| {
//            destroyExpression(alloc, e.left);
//            destroyExpression(alloc, e.right);
//        },
//        .Prefix => |e| {
//            destroyExpression(alloc, e.right);
//        },
//        .Integer, .Ident => {
//            return;
//        },
//    }
//    alloc.destroy(ptr);
//}
//
//fn destroyStatement(alloc: std.mem.Allocator, ptr: *Statement) void {
//    switch (ptr.*) {
//        .Expr => |s| {
//            destroyExpression(alloc, s.expression);
//        },
//        .Return => |s| {
//            destroyExpression(alloc, s.returnValue);
//        },
//        .Let => |s| {
//            destroyExpression(alloc, s.value);
//        },
//    }
//}
//
