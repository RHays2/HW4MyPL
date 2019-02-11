#!/usr/bin/python3
#
# Author: Joshua Go
# Course: CPSC 326, Spring 2019
# Assignment: 3
# Description:
#   This is a syntax checker that uses recursive descent parsing. It takes in a source file written in MyPL and reports
#   the first error that it finds, or nothing if the input is syntactically well-formed.
# ----------------------------------------------------------------------
import mypl_error as error
import mypl_lexer as lexer
import mypl_token as token

class Parser(object):


    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = None

    def parse(self):
        """succeeds if program is syntactically well-formed"""
        self.__advance()
        self.__stmts()
        self.__eat(token.EOS, 'expecting end of file')

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
    def __stmts(self):
        """"<stmts> ::= <stmt> <stmts> | e"""
        if self.current_token.tokentype != token.EOS:
            self.__stmt()
            self.__stmts()

    # statement checker
    def __stmt(self):
        """<stmt> ::= <sdecl> | <fdecl> | <bstmt>"""
        if self.current_token.tokentype == token.STRUCTTYPE:
            self.__sdecl()
        elif self.current_token.tokentype == token.FUN:
            self.__fdecl()
        else:
            self.__bstmt()

    # struct declaration
    def __sdecl(self):
        self.__advance()
        self.__eat(token.ID, "Missing 'id'")
        self.__vdecls()
        self.__eat(token.END, "Missing 'end' statement")

    # function declaration
    def __fdecl(self):
        self.__eat(token.FUN, "Missing 'fun' declaration for function")
        if self.current_token.tokentype == token.NIL:
            self.__advance()
        else:
            self.__type()
        self.__eat(token.ID, "Missing function ID name")
        self.__eat(token.LPAREN, "Missing left parenthesis")
        self.__params()
        self.__eat(token.RPAREN, "Missing right parenthesis")
        self.__bstmts()
        self.__eat(token.END, "Missing 'end' statement")

    # grammar for function parameters
    def __params(self):
        if self.current_token.tokentype == token.ID:
            self.__eat(token.ID, "Missing variable name")
            self.__eat(token.COLON, "Missing colon after ID")
            self.__type()
            while self.current_token.tokentype == token.COMMA:
                self.__advance()
                self.__eat(token.ID, "Missing ID after comma")
                self.__eat(token.COLON, "Missing colon after ID")
                self.__type()

    # boolean statement
    def __bstmt(self):
        expr_tokens = [token.ID, token.STRINGVAL, token.INTVAL, token.BOOLVAL, token.FLOATVAL, token.NIL, token.NEW,
                  token.LPAREN]
        if self.current_token.tokentype == token.VAR:
            self.__vdecl()
        elif self.current_token.tokentype == token.SET:
            self.__assign()
        elif self.current_token.tokentype == token.IF:
            self.__cond()
        elif self.current_token.tokentype == token.WHILE:
            self.__while()
        elif self.current_token.tokentype in expr_tokens:
            self.__expr()
            self.__eat(token.SEMICOLON, "Missing semicolon")
        elif self.current_token.tokentype == token.RETURN:
            self.__exit()
        else:
            raise error.MyPLError("Invalid statement", self.current_token.line, self.current_token.column)

    # grammar for return statements
    def __exit(self):
        expr_tokens = [token.ID, token.STRINGVAL, token.INTVAL, token.BOOLVAL, token.FLOATVAL, token.NIL, token.NEW,
                       token.LPAREN]
        self.__eat(token.RETURN, "Missing 'return' statement")
        if self.current_token.tokentype in expr_tokens:
            self.__expr()
        self.__eat(token.SEMICOLON, "Missing semicolon after 'return' statement")

    # grammar for while-loops
    def __while(self):
        self.__eat(token.WHILE, "Missing 'while' statement for loop")
        self.__bexpr()
        self.__eat(token.DO, "Missing 'do' statement for loop")
        self.__bstmts()
        self.__eat(token.END, "Missing 'end' statement for loop")

    # grammar for the if-else conditional statements
    def __cond(self):
        self.__eat(token.IF, "Missing 'if' statement")
        self.__bexpr()
        self.__eat(token.THEN, "Missing 'then' statement")
        self.__bstmts()
        self.__condt()
        self.__eat(token.END, "Missing 'end' statement")

    # grammar for conditional tail
    def __condt(self):
        if self.current_token.tokentype == token.ELIF:
            self.__advance()
            self.__bexpr()
            self.__eat(token.THEN, "Missing 'then' statement")
            self.__bstmts()
            self.__condt()
        elif self.current_token.tokentype == token.ELSE:
            self.__advance()
            self.__bstmts()

    def __bstmts(self):
        bstmt_tokens = [token.WHILE, token.RETURN, token.IF, token.VAR, token.ID, token.STRINGVAL, token.INTVAL,
                        token.BOOLVAL, token.FLOATVAL, token.NIL, token.NEW, token.LPAREN]
        if self.current_token.tokentype in bstmt_tokens:
            self.__bstmt()
            self.__bstmts()

    # grammar for boolean expressions
    def __bexpr(self):
        if self.current_token.tokentype == token.NOT:
            self.__advance()
            self.__bexpr()
            self.__bexprt()
        elif self.current_token.tokentype == token.LPAREN:
            self.__advance()
            self.__bexpr()
            self.__eat(token.RPAREN, "Missing right paren")
            self.__bconnct()
        else:
            self.__expr()
            self.__bexprt()

    # tail for bexpr()
    def __bexprt(self):
        boolrel = [token.EQUAL, token.LESS_THAN, token.GREATER_THAN, token.LESS_THAN_EQUAL, token.GREATER_THAN_EQUAL,
                   token.NOT_EQUAL]
        if self.current_token.tokentype in boolrel:
            self.__advance()
            self.__expr()
        self.__bconnct()

    # grammar on how boolean variables connect
    def __bconnct(self):
        if self.current_token.tokentype == token.AND:
            self.__advance()
            self.__bexpr()
        elif self.current_token.tokentype == token.OR:
            self.__advance()
            self.__bexpr()

    # function that defines the grammar to assign values to variables
    def __assign(self):
        self.__eat(token.SET, "Missing 'set' variable")
        self.__lvalue()
        self.__eat(token.ASSIGN, "Missing assign '=' variable")
        self.__expr()
        self.__eat(token.SEMICOLON, "Missing semicolon")

    # left value grammar
    def __lvalue(self):
        self.__eat(token.ID, "Missing 'ID' variable")
        while self.current_token.tokentype == token.DOT:
            self.__advance()
            self.__eat(token.ID, "Missing 'ID' variable")

    # value declaration statement
    def __vdecls(self):
        if self.current_token.tokentype == token.VAR:
            self.__vdecl()
            self.__vdecls()

    # value declaration
    def __vdecl(self):
        self.__eat(token.VAR, "Missing 'var' declaration")
        self.__eat(token.ID, "Missing 'ID' declaration")
        self.__tdecl()
        self.__eat(token.ASSIGN, "Missing assign '=' declaration")
        self.__expr()
        self.__eat(token.SEMICOLON, "Missing semicolon")

    #   tail declaration
    def __tdecl(self):
        if self.current_token.tokentype == token.COLON:
            self.__advance()
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
            self.__advance()
            self.__expr()
            self.__eat(token.RPAREN, "Missing right parenthesis")
        else:
            self.__rvalue()
        mathrels = [token.PLUS, token.MINUS, token.DIVIDE, token.MULTIPLY, token.MODULO]
        if self.current_token.tokentype in mathrels:
            self.__advance()
            self.__expr()

    # defines right values for expressions
    def __rvalue(self):
        if self.current_token.tokentype == token.STRINGVAL:
            self.__advance()
        elif self.current_token.tokentype == token.INTVAL:
            self.__advance()
        elif self.current_token.tokentype == token.BOOLVAL:
            self.__advance()
        elif self.current_token.tokentype == token.FLOATVAL:
            self.__advance()
        elif self.current_token.tokentype == token.NIL:
            self.__advance()
        elif self.current_token.tokentype == token.NEW:
            self.__advance()
            self.__eat(token.ID, "Missing 'ID'")
        elif self.current_token.tokentype == token.ID:
            self.__idrval()
        else:
            self.__error("Missing variable declaration")

    # defines values for ID
    def __idrval(self):
        if self.current_token.tokentype == token.ID:
            self.__advance()
            if self.current_token.tokentype == token.DOT:
                while self.current_token.tokentype == token.DOT:
                    self.__advance()
                    self.__eat(token.ID, "Missing 'ID'")
            elif self.current_token.tokentype == token.LPAREN:
                self.__eat(token.LPAREN, "Missing left parenthesis")
                self.__exprlist()
                self.__eat(token.RPAREN, "Missing right parenthesis")

    # function contains grammar for expressions
    def __exprlist(self):
        # tokens that can start an expression
        types = [token.STRINGVAL, token.INTVAL, token.FLOATVAL, token.BOOLVAL, token.ID, token.LPAREN]
        if self.current_token.tokentype in types:
            self.__expr()
            while self.current_token.tokentype == token.COMMA:
                self.__advance()
                self.__expr()



