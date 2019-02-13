#!/usr/bin/python3
#
# Author: Joshua Go
# Course: CPSC 326, Spring 2019
# Assignment: 4
# Description:
#   This is a syntax checker that uses recursive descent parsing. It takes in a source file written in MyPL and reports
#   the first error that it finds, or nothing if the input is syntactically well-formed.
# ----------------------------------------------------------------------
import mypl_error as error
import mypl_lexer as lexer
import mypl_token as token
import mypl_ast as ast

class Parser(object):


    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = None

    def parse(self):
        """succeeds if program is syntactically well-formed"""
        stmts_node = ast.StmtList()
        self.__advance()
        self.__stmts(stmts_node)
        self.__eat(token.EOS, 'expecting end of file')
        return stmts_node

    def __advance(self):
        self.current_token = self.lexer.next_token()

    def __eat(self, tokentype, error_msg):
        if self.current_token.tokentype == tokentype:
            self.__advance()
        else:
            self.__error(error_msg)

    def __error(self, error_msg):
        s = error_msg + ', found "' + self.current_token.lexeme + '" in parser'
        l = self.current_token.line
        c = self.current_token.column
        raise error.MyPLError(error_msg, l, c)

    # Beginning of recursive descent functions
    def __stmts(self, stmts_node):
        """"<stmts> ::= <stmt> <stmts> | e"""
        if self.current_token.tokentype != token.EOS:
            self.__stmt(stmts_node)
            self.__stmts(stmts_node)

    # statement checker
    def __stmt(self, stmts_node):
        """<stmt> ::= <sdecl> | <fdecl> | <bstmt>"""
        if self.current_token.tokentype == token.STRUCTTYPE:
            self.__sdecl(stmts_node)
        elif self.current_token.tokentype == token.FUN:
            self.__fdecl(stmts_node)
        else:
            stmts_node.smts.append(self.__bstmt())

    # struct declaration
    def __sdecl(self, stmts_node):
        struct_decl_stmt_node = ast.StructDeclStmt()
        self.__advance()
        struct_decl_stmt_node.struct_id = self.current_token.tokentype
        self.__eat(token.ID, "Missing 'id'")
        self.__vdecls(struct_decl_stmt_node)
        self.__eat(token.END, "Missing 'end' statement")
        stmts_node.stmt.append(struct_decl_stmt_node)    # add new node to StmtList

    # function declaration
    def __fdecl(self, stmts_node):
        fun_decl_stmt_node = ast.FunDeclStmt()
        self.__eat(token.FUN, "Missing 'fun' declaration for function")
        if self.current_token.tokentype == token.NIL:
            self.__advance()
        else:
            self.__type()
        fun_decl_stmt_node.fun_name = self.current_token
        self.__eat(token.ID, "Missing function ID name")
        self.__eat(token.LPAREN, "Missing left parenthesis")
        self.__params(fun_decl_stmt_node)
        self.__eat(token.RPAREN, "Missing right parenthesis")
        self.__bstmts(fun_decl_stmt_node.stmt_list)
        self.__eat(token.END, "Missing 'end' statement")
        stmts_node.stmt.append(fun_decl_stmt_node)  # add new node to StmtList

    # grammar for function parameters
    def __params(self, fun_decl_stmt_node):
        if self.current_token.tokentype == token.ID:
            fun_param_node = ast.FunParam()
            fun_param_node.param_name = self.current_token
            self.__eat(token.ID, "Missing variable name")
            self.__eat(token.COLON, "Missing colon after ID")
            fun_param_node.param_type = self.current_token.tokentype
            self.__type()
            fun_decl_stmt_node.params.append(fun_param_node)
            while self.current_token.tokentype == token.COMMA:
                self.__advance()
                fun_param_node = ast.FunParam()
                fun_param_node.param_name = self.current_token
                self.__eat(token.ID, "Missing ID after comma")
                self.__eat(token.COLON, "Missing colon after ID")
                fun_param_node.param_type = self.current_token.tokentype
                self.__type()
                fun_decl_stmt_node.params.append(fun_param_node)

    # boolean statement
    def __bstmt(self):
        expr_tokens = [token.ID, token.STRINGVAL, token.INTVAL, token.BOOLVAL, token.FLOATVAL, token.NIL, token.NEW,
                  token.LPAREN]
        if self.current_token.tokentype == token.VAR:
            return self.__vdecl()
        elif self.current_token.tokentype == token.SET:
            return self.__assign()
        elif self.current_token.tokentype == token.IF:
            return self.__cond()
        elif self.current_token.tokentype == token.WHILE:
            return self.__while()
        elif self.current_token.tokentype in expr_tokens:
            expr_stmt_node = ast.ExprStmt()
            expr_stmt_node.expr = self.__expr()
            self.__eat(token.SEMICOLON, "Missing semicolon")
            return expr_stmt_node
        elif self.current_token.tokentype == token.RETURN:
            return self.__exit()
        else:
            raise error.MyPLError("Invalid statement", self.current_token.line, self.current_token.column)

    # grammar for return statements
    def __exit(self):
        return_stmt_node = ast.ReturnStmt()
        expr_tokens = [token.ID, token.STRINGVAL, token.INTVAL, token.BOOLVAL, token.FLOATVAL, token.NIL, token.NEW,
                       token.LPAREN]
        return_stmt_node.return_token = self.current_token.tokentype
        self.__eat(token.RETURN, "Missing 'return' statement")
        if self.current_token.tokentype in expr_tokens:
            return_stmt_node.return_expr = self.__expr()
        self.__eat(token.SEMICOLON, "Missing semicolon after 'return' statement")
        return return_stmt_node

    # grammar for while-loops
    def __while(self):
        while_stmt_node = ast.WhileStmt()
        self.__eat(token.WHILE, "Missing 'while' statement for loop")
        while_stmt_node.bool_expr = self.__bexpr()
        self.__eat(token.DO, "Missing 'do' statement for loop")
        self.__bstmts(while_stmt_node.stmt_list)
        self.__eat(token.END, "Missing 'end' statement for loop")
        return while_stmt_node

    # grammar for the if-else conditional statements
    def __cond(self):
        if_stmt_node = ast.IfStmt()
        self.__eat(token.IF, "Missing 'if' statement")
        if_stmt_node.if_part.bool_expr = self.__bexpr()
        self.__eat(token.THEN, "Missing 'then' statement")
        self.__bstmts(if_stmt_node.else_stmts)
        self.__condt(if_stmt_node)
        self.__eat(token.END, "Missing 'end' statement")
        return if_stmt_node

    # grammar for conditional tail
    def __condt(self, if_stmt_node):
        basic_if_node = ast.BasicIf()
        if self.current_token.tokentype == token.ELIF:
            self.__advance()
            basic_if_node.bool_expr = self.__bexpr()
            self.__eat(token.THEN, "Missing 'then' statement")
            self.__bstmts(basic_if_node.stmt_list)
            self.__condt(if_stmt_node)
        elif self.current_token.tokentype == token.ELSE:
            if_stmt_node.has_else = True
            self.__advance()
            self.__bstmts(basic_if_node.stmt_list)
        if_stmt_node.elseifs.append(basic_if_node)

    def __bstmts(self, stmts_node):
        bstmt_tokens = [token.WHILE, token.RETURN, token.IF, token.SET, token.VAR, token.ID, token.STRINGVAL,
                        token.INTVAL, token.BOOLVAL, token.FLOATVAL, token.NIL, token.NEW, token.LPAREN]
        if self.current_token.tokentype in bstmt_tokens:
            stmts_node.stmt.append(self.__bstmt())
            self.__bstmts(stmts_node)

    # grammar for boolean expressions
    def __bexpr(self):
        bool_expr_node = ast.BoolExpr()
        if self.current_token.tokentype == token.NOT:
            bool_expr_node.negated = True
            self.__advance()
            bool_expr_node.rest = self.__bexpr()
            self.__bexprt(bool_expr_node)
        elif self.current_token.tokentype == token.LPAREN:
            self.__advance()
            bool_expr_node.rest = self.__bexpr()
            self.__eat(token.RPAREN, "Missing right paren")
            self.__bconnct(bool_expr_node)
        else:
            bool_expr_node.first_expr = self.__expr()
            self.__bexprt(bool_expr_node)
        return bool_expr_node

    # tail for bexpr()
    def __bexprt(self, bool_expr_node):
        boolrel = [token.EQUAL, token.LESS_THAN, token.GREATER_THAN, token.LESS_THAN_EQUAL, token.GREATER_THAN_EQUAL,
                   token.NOT_EQUAL]
        if self.current_token.tokentype in boolrel:
            bool_expr_node.bool_rel = self.current_token
            self.__advance()
            bool_expr_node.second_expr = self.__expr()
        self.__bconnct(bool_expr_node)

    # grammar on how boolean variables connect
    def __bconnct(self, bool_expr_node):
        if self.current_token.tokentype == token.AND:
            bool_expr_node.bool_connector = self.current_token
            self.__advance()
            bool_expr_node.rest = self.__bexpr()
        elif self.current_token.tokentype == token.OR:
            bool_expr_node.bool_connector = self.current_token
            self.__advance()
            bool_expr_node.rest = self.__bexpr()

    # function that defines the grammar to assign values to variables
    def __assign(self):
        assign_stmt_node = ast.AssignStmt()
        self.__eat(token.SET, "Missing 'set' variable")
        self.__lvalue(assign_stmt_node)
        self.__eat(token.ASSIGN, "Missing assign '=' variable")
        assign_stmt_node.rhs = self.__expr()
        self.__eat(token.SEMICOLON, "Missing semicolon")
        return assign_stmt_node

    # left value grammar
    def __lvalue(self, assign_stmt_node):
        lvalue_node = ast.LValue()
        lvalue_node.path.append(self.current_token)
        self.__eat(token.ID, "Missing 'ID' variable")
        while self.current_token.tokentype == token.DOT:
            self.__advance()
            lvalue_node.path.append(self.current_token)
            self.__eat(token.ID, "Missing 'ID' variable")
        assign_stmt_node.lhs = lvalue_node

    # value declaration statement
    def __vdecls(self, struct_decl_stmt_node):
        if self.current_token.tokentype == token.VAR:
            struct_decl_stmt_node.var_decls.append(self.__vdecl())
            self.__vdecls(struct_decl_stmt_node)

    # value declaration
    def __vdecl(self):
        var_decl_stmt_node = ast.VarDeclStmt()
        self.__eat(token.VAR, "Missing 'var' declaration")
        var_decl_stmt_node.var_id = self.current_token
        self.__eat(token.ID, "Missing 'ID' declaration")
        self.__tdecl(var_decl_stmt_node)
        self.__eat(token.ASSIGN, "Missing assign '=' declaration")
        var_decl_stmt_node.var_expr = self.__expr()
        self.__eat(token.SEMICOLON, "Missing semicolon")
        return var_decl_stmt_node

    #   tail declaration
    def __tdecl(self, var_decl_stmt_node):
        if self.current_token.tokentype == token.COLON:
            self.__advance()
            var_decl_stmt_node.var_type = self.current_token.tokentype
            self.__type()

    # function that defines variable type grammar
    def __type(self):
        if self.current_token.tokentype == token.ID:
            self.__advance()
        elif self.current_token.tokentype == token.INTTYPE:
            self.__advance()
        elif self.current_token.tokentype == token.FLOATTYPE:
            self.__advance()
        elif self.current_token.tokentype == token.BOOLTYPE:
            self.__advance()
        elif self.current_token.tokentype == token.STRINGTYPE:
            self.__advance()
        else:
            self.__error("Variable type not valid")

    # function for defining expressions
    def __expr(self):
        if self.current_token.tokentype == token.LPAREN:
            complex_expr_node = ast.ComplexExpr()
            self.__advance()
            complex_expr_node.first_operand = self.__expr()
            self.__eat(token.RPAREN, "Missing right parenthesis")
        else:   # simple expression
            simple_expr_node = ast.SimpleExpr()
            self.__rvalue(simple_expr_node)
            return simple_expr_node
        mathrels = [token.PLUS, token.MINUS, token.DIVIDE, token.MULTIPLY, token.MODULO]
        if self.current_token.tokentype in mathrels:
            complex_expr_node.math_rel = self.current_token
            self.__advance()
            complex_expr_node.rest = self.__expr()
            return complex_expr_node

    # defines right values for expressions
    def __rvalue(self, simple_expr_node):
        if self.current_token.tokentype == token.STRINGVAL:
            simple_rvalue_node = ast.SimpleRValue()
            simple_rvalue_node.val = self.current_token
            simple_expr_node.term = simple_rvalue_node
            self.__advance()
        elif self.current_token.tokentype == token.INTVAL:
            simple_rvalue_node = ast.SimpleRValue()
            simple_rvalue_node.val = self.current_token
            simple_expr_node.term = simple_rvalue_node
            self.__advance()
        elif self.current_token.tokentype == token.BOOLVAL:
            simple_rvalue_node = ast.SimpleRValue()
            simple_rvalue_node.val = self.current_token
            simple_expr_node.term = simple_rvalue_node
            self.__advance()
        elif self.current_token.tokentype == token.FLOATVAL:
            simple_rvalue_node = ast.SimpleRValue()
            simple_rvalue_node.val = self.current_token
            simple_expr_node.term = simple_rvalue_node
            self.__advance()
        elif self.current_token.tokentype == token.NIL:
            simple_rvalue_node = ast.SimpleRValue()
            simple_rvalue_node.val = self.current_token
            simple_expr_node.term = simple_rvalue_node
            self.__advance()
        elif self.current_token.tokentype == token.NEW:
            self.__advance()
            new_rvalue_node = ast.NewRValue()
            new_rvalue_node.struct_type = self.current_token
            self.__eat(token.ID, "Missing 'ID'")
            simple_expr_node.term = new_rvalue_node
        elif self.current_token.tokentype == token.ID:
            self.__idrval(simple_expr_node)
        else:
            self.__error("Missing variable declaration")

    # defines values for ID
    def __idrval(self, simple_expr_node):
        if self.current_token.tokentype == token.ID:
            call_rvalue_node = ast.CallRValue()
            call_rvalue_node.fun = self.current_token

            id_rvalue_node = ast.IDRvalue()
            id_rvalue_node.path.append(self.current_token)
            self.__advance()
            if self.current_token.tokentype == token.DOT:
                while self.current_token.tokentype == token.DOT:
                    self.__advance()
                    id_rvalue_node.path.append(self.current_token)
                    self.__eat(token.ID, "Missing 'ID'")
                simple_expr_node.term = id_rvalue_node
            elif self.current_token.tokentype == token.LPAREN:
                self.__eat(token.LPAREN, "Missing left parenthesis")
                self.__exprlist(call_rvalue_node)
                self.__eat(token.RPAREN, "Missing right parenthesis")

    # function contains grammar for expressions
    def __exprlist(self, call_rvalue_node):
        # tokens that can start an expression
        types = [token.STRINGVAL, token.INTVAL, token.FLOATVAL, token.BOOLVAL, token.ID, token.LPAREN]
        if self.current_token.tokentype in types:
            call_rvalue_node.args.append(self.__expr())
            while self.current_token.tokentype == token.COMMA:
                self.__advance()
                call_rvalue_node.args.append(self.__expr())



