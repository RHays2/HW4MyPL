#!/usr/bin/python3
#
# Author: Joshua Go
# Course: CPSC 326, Spring 2019
# Assignment: 4
import mypl_token as token
import mypl_ast as ast

class PrintVisitor(ast.Visitor):
    """An AST pretty printer"""
    def __init__(self, output_stream):
        self.indent = 0 # to increase/decrease indent level
        self.output_stream = output_stream # where printing to

    def __indent(self):
        """Get default indent of four spaces"""
        return ' ' * self.indent

    def __write(self, msg):
        self.output_stream.write(msg)

    def visit_stmt_list(self, stmt_list):
        for stmt in stmt_list.stmts:
            stmt.accept(self)

    def visit_expr_stmt(self, expr_stmt):
        self.__write(self.__indent())
        expr_stmt.expr.accept(self)
        self.__write(';\n')

    def visit_var_decl_stmt(self, var_decl):
        self.__write('var')
        self.__write(' ')
        self.__write(var_decl.var_id)
        if var_decl.var_type != None:   # if there is a type for the var
            self.__write(':')
            self.__write(' ')
            if var_decl.var_type == token.STRINGTYPE:
                self.__write('string')
            elif var_decl.var_type == token.INTTYPE:
                self.__write('int')
            elif var_decl.var_type == token.FLOATTYPE:
                self.__write('float')
            elif var_decl.var_type == token.BOOLTYPE:
                self.__write('bool')
            elif var_decl.var_type == token.NIL:
                self.__write('nil')
        self.__write(' ')
        self.__write('=')
        self.__write(' ')
        var_decl.var_expr.accept(self)
        self.__write(';')
        self.__write('\n')

    def visit_assign_stmt(self, assign_stmt):
        self.__write('set')
        self.__write(' ')
        assign_stmt.lhs.accept(self)    # lvalue node
        self.__write(' ')
        self.__write('=')
        self.__write(' ')
        assign_stmt.rhs.accept(self)    # Expr node
        self.__write(';')
        self.__write('\n')

    def visit_struct_decl_stmt(self, struct_decl):
        self.__write('\n')
        self.__write('struct')
        self.__write(' ')
        self.__write(struct_decl.struct_id)
        self.__write('\n')
        for i in struct_decl.var_decls:
            self.__write('    ')
            i.accept(self)
        self.__write('end')
        self.__write('\n')
        self.__write('\n')

    def visit_fun_decl_stmt(self, fun_decl):
        self.__write('\n')
        self.__write('fun')
        self.__write(' ')
        if fun_decl.return_type == token.STRINGTYPE:
            self.__write('string')
        elif fun_decl.return_type == token.INTTYPE:
            self.__write('int')
        elif fun_decl.return_type == token.FLOATTYPE:
            self.__write('float')
        elif fun_decl.return_type == token.BOOLTYPE:
            self.__write('bool')
        else:
            self.__write('nil')
        self.__write(' ')
        self.__write(fun_decl.fun_name)
        self.__write('(')
        j = 0
        for i in fun_decl.params:
            if j > 0:
                self.__write(',')
                self.__write(' ')
            i.accept(self)
            j = j + 1
        self.__write(')')
        self.__write('\n')
        j = 0
        for k in fun_decl.stmt_list.stmts:
            self.__write('    ')
            k.accept(self)

        self.__write('end')
        self.__write('\n')
        self.__write('\n')

    def visit_return_stmt(self, return_stmt):
        self.__write('return')
        if return_stmt.return_expr != None:
            self.__write(' ')
            return_stmt.return_expr.accept(self)
        self.__write(';')
        self.__write('\n')

    def visit_if_stmt(self, if_stmt):
        self.__write('if')
        if_stmt.if_part.bool_expr.accept(self)
        self.__write('\n')
        for k in if_stmt.if_part.stmt_list.stmts:
            self.__write('    ')
            k.accept(self)


    def visit_fun_param(self, fun_param):
        self.__write(fun_param.param_name)
        self.__write(':')
        self.__write(' ')
        self.__write(fun_param.param_type)

    def visit_lvalue(self, lval):
        i = 0
        self.__write(lval.path[i])
        i = i + 1
        while i < len(lval.path):
            self.__write(".")
            self.__write(lval.path[i])
            i = i + 1

    def visit_call_rvalue(self, call_rvalue):
        self.__write(call_rvalue.fun)
        self.__write('(')
        for i in call_rvalue.args:
            i.accept(self)
        self.__write(')')

    def visit_id_rvalue(self, id_rvalue):
        i = 0
        self.__write(id_rvalue.path[i])
        i = i + 1
        while i < len(id_rvalue.path):
            self.__write(".")
            self.__write(i)
            i = i + 1

    def visit_simple_expr(self, simple_expr):
        simple_expr.term.accept(self)

    def visit_simple_rvalue(self, simple_rvalue):
        self.__write(simple_rvalue.val)

    def visit_complex_expr(self, complex_expr):
        self.__write('(')
        complex_expr.first_operand.accept(self)
        self.__write(' ')
        self.__write(complex_expr.math_rel)
        self.__write(' ')
        complex_expr.rest.accept(self)
        self.__write(')')

    def visit_while_stmt(self, while_stmt):
        self.__write('while')
        self.__write('\n')
