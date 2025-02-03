const std = @import("std");
const Token = @import("../tokens.zig").Token;
const AST = @This();

pub const Precedence = enum {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

pub const Statement = union(enum) {
    Let: LetStatement,
    Return: ReturnStatement,
    Expr: ExpressionStatement,
    //Other statement variants
    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            .Let => |stmt| stmt.tokenLiteral(),
            .Return => |stmt| stmt.tokenLiteral(),
            .Expr => |stmt| stmt.tokenLiteral(),
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
    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            .IntegerExpression => |expr| expr.tokenLiteral,
            //.Return => |stmt| stmt.tokenLiteral(),
        };
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: ?Expression,
    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.Literal;
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: ?Expression,
    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.Literal;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    returnValue: ?Expression,
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

    pub fn destroy(self: *Self) void {
        self.statements.deinit();
        self.alloc.destroy(self);
    }
};
