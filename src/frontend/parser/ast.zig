const AST = @This();

const std = @import("std");

const Token = @import("../tokens.zig").Token;
const TokenType = @import("../tokens.zig").TokenType;

pub const Node = union(enum) {
    program: *Program,
    statement: *Statement,
    expression: *Expression,
    pub fn toString(self: Node) []const u8 {
        switch (self) {
            inline else => |case| case.toString(),
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    pub fn tokenLiteral(self: Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        } else {
            return "";
        }
    }
    pub fn print(self: Program) void {
        if (self.statements.items.len > 0) {
            for (self.statements.items) |stmt| {
                stmt.print();
            }
        } else {
            std.debug.print("No statemenets to print.\n", .{});
        }
    }
};

pub const Statement = union(enum) {
    Let: LetStatement,
    Return: ReturnStatement,
    Expr: ExpressionStatement,
    Print: PrintStatement,
    Block: BlockStatement,
    //Other statement variants
    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            inline else => |stmt| stmt.tokenLiteral(),
        };
    }

    pub fn print(self: Statement) void {
        switch (self) {
            inline else => |stmt| stmt.print(),
        }
    }
};

pub const Expression = union(enum) {
    Integer: IntegerExpression,
    Ident: Identifier,
    Prefix: PrefixExpression,
    Infix: InfixExpression,
    If: IfExpression,
    Bool: BooleanExpression,
    Function: FunctionLiteral,
    Call: CallExpression,
    String: StringLiteral,
    Array: ArrayLiteral,
    Index: IndexExpression,
    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            inline else => |case| case.tokenLiteral(),
        };
    }

    pub fn print(self: Expression) void {
        switch (self) {
            inline else => |case| case.print(),
        }
    }
};

// OPERATORS //

pub const PrefixOperator = enum {
    BANG,
    MINUS,
    pub fn fromString(str: []const u8) PrefixOperator {
        if (std.mem.eql(u8, str, "!")) return PrefixOperator.BANG;
        if (std.mem.eql(u8, str, "-")) return PrefixOperator.MINUS;
        unreachable;
    }
    pub fn toString(self: PrefixOperator) []const u8 {
        switch (self) {
            .BANG => return "!",
            .MINUS => return "-",
        }
    }
};

pub const InfixOperator = enum {
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    LESS_THAN,
    GREATER_THAN,
    EQUAL,
    NOT_EQUAL,
    pub fn fromString(str: []const u8) InfixOperator {
        if (std.mem.eql(u8, str, "+")) return InfixOperator.PLUS;
        if (std.mem.eql(u8, str, "-")) return InfixOperator.MINUS;
        if (std.mem.eql(u8, str, "*")) return InfixOperator.MULTIPLY;
        if (std.mem.eql(u8, str, "/")) return InfixOperator.DIVIDE;
        if (std.mem.eql(u8, str, "<")) return InfixOperator.LESS_THAN;
        if (std.mem.eql(u8, str, ">")) return InfixOperator.GREATER_THAN;
        if (std.mem.eql(u8, str, "==")) return InfixOperator.EQUAL;
        if (std.mem.eql(u8, str, "!=")) return InfixOperator.NOT_EQUAL;
        unreachable;
    }

    pub fn toString(self: InfixOperator) []const u8 {
        switch (self) {
            .PLUS => return "+",
            .MINUS => return "-",
            .MULTIPLY => return "*",
            .DIVIDE => return "/",
            .LESS_THAN => return "<",
            .GREATER_THAN => return ">",
            .EQUAL => return "==",
            .NOT_EQUAL => return "!=",
        }
    }
};

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

// STATEMENTS //
pub const PrintStatement = struct {
    token: Token,
    value: *Expression,
    pub fn tokenLiteral(self: PrintStatement) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: PrintStatement) void {
        std.debug.print("{s} : ", .{self.token.Literal});
        self.value.print();
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(Statement),
    pub fn tokenLiteral(self: BlockStatement) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: BlockStatement) void {
        for (self.statements.items) |stmt| {
            stmt.print();
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: *Expression,
    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: LetStatement) void {
        std.debug.print("{s} ", .{self.tokenLiteral()});
        self.name.print();
        std.debug.print(" = ", .{});
        self.value.print();
        std.debug.print(";\n", .{});
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression,
    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: ExpressionStatement) void {
        self.expression.print();
    }
};

pub const ReturnStatement = struct {
    token: Token,
    returnValue: *Expression,
    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: ReturnStatement) void {
        std.debug.print("{s} ", .{self.tokenLiteral()});
        self.returnValue.print();
        std.debug.print("\n", .{});
    }
};

// EXPRESSIONS //
pub const PrefixExpression = struct {
    token: Token,
    operator: PrefixOperator,
    right: *Expression,
    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: PrefixExpression) void {
        std.debug.print("(", .{});
        std.debug.print("{any}", .{self.operator});
        self.right.print();
        std.debug.print(")\n", .{});
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: InfixOperator,
    right: *Expression,
    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: InfixExpression) void {
        std.debug.print("(", .{});
        self.left.print();
        std.debug.print(" {any} ", .{self.operator});
        self.right.print();
        std.debug.print(")\n", .{});
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,
    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: Identifier) void {
        std.debug.print("{s}\n", .{self.value});
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: std.ArrayList(Identifier),
    body: *BlockStatement,
    pub fn tokenLiteral(self: FunctionLiteral) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: FunctionLiteral) void {
        std.debug.print("{s}", .{self.tokenLiteral()});
        std.debug.print("(", .{});
        for (self.parameters.items, 0..) |arg, idx| {
            std.debug.print("param {d} : ", .{idx});
            arg.print();
            std.debug.print("\n", .{});
        }
        std.debug.print(")\n", .{});
        self.body.print();
    }
};

pub const StringLiteral = struct {
    token: Token,
    value: []const u8,
    pub fn tokenLiteral(self: StringLiteral) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: StringLiteral) void {
        std.debug.print("{s}\n", .{self.value});
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *Expression,
    args: std.ArrayList(Expression),
    pub fn tokenLiteral(self: CallExpression) []const u8 {
        return self.token.Length;
    }

    pub fn print(self: CallExpression) void {
        self.function.print();
        std.debug.print("(", .{});
        for (self.args.items, 0..) |arg, idx| {
            std.debug.print("arg {d} : ", .{idx});
            arg.print();
            std.debug.print("\n", .{});
        }
        std.debug.print(")\n", .{});
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,
    pub fn tokenLiteral(self: IfExpression) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: IfExpression) void {
        std.debug.print("if ", .{});
        self.condition.print();
        std.debug.print(" ", .{});
        self.consequence.print();

        if (self.alternative != null) {
            std.debug.print("else ", .{});
            self.alternative.?.print();
        }
    }
};

pub const IntegerExpression = struct {
    value: i64,
    token: Token,
    pub fn tokenLiteral(self: IntegerExpression) []const u8 {
        return self.token.Literal;
    }
    pub fn print(self: IntegerExpression) void {
        std.debug.print("{s}", .{self.token.Literal});
    }
};

pub const BooleanExpression = struct {
    value: bool,
    token: Token,
    pub fn tokenLiteral(self: BooleanExpression) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: BooleanExpression) void {
        std.debug.print("{s}", .{self.token.Literal});
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: std.ArrayList(Expression),
    pub fn tokenLiteral(self: ArrayLiteral) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: ArrayLiteral) void {
        std.debug.print("{s}", .{self.token.Literal});
    }
};

pub const IndexExpression = struct {
    token: Token,
    left: *Expression,
    index: *Expression,
    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.Literal;
    }

    pub fn print(self: IndexExpression) void {
        std.debug.print("{s}", .{self.token.Literal});
    }
};
