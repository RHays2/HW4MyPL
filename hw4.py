#!/usr/bin/python3
#
# Author:
# Assignment: 4
# Description:
# Simple script to execute the MyPL parser and pretty printer.
#----------------------------------------------------------------------
import mypl_error as error
import mypl_lexer as lexer
import mypl_token as token
import mypl_parser as parser
import mypl_ast as ast
import mypl_print_visitor as ast_printer
import sys

def main(filename):
    try:
        file_stream = open(filename, 'r')
        hw4(file_stream)
        file_stream.close()
    except FileNotFoundError:
        sys.exit('invalid filename %s' % filename)
    except error.MyPLError as e:
        file_stream.close()
        sys.exit(e)

def hw4(file_stream):
    the_lexer = lexer.Lexer(file_stream)
    the_parser = parser.Parser(the_lexer)
    stmt_list = the_parser.parse()
    print_visitor = ast_printer.PrintVisitor(sys.stdout)
    stmt_list.accept(print_visitor)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit('Usage: %s file' % sys.argv[0])
    main(sys.argv[1])