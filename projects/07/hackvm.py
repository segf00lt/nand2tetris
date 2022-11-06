#!/usr/bin/env python3

from sys import argv
import re

# tokens
# arith
ADD = 1
SUB = 2
AND = 3
OR  = 4
EQ  = 5
GT  = 6
LT  = 7
NEG = 8
NOT = 9
# mem
PUSH = 10
POP = 11
# flow
LABEL = 12
GOTO = 13
IFGOTO = 14
# func
FUNC = 15
CALL = 16
RET = 17
# misc
NUM = 18
SYM = 19
SEG = 20

segment_names = ['argument','local','this','that','temp','pointer','static','constant']
symbol_table = {}
stack = []
global_stack = []

def lex(text : str):
    for _,s in enumerate(re.sub('(^\n)|(//.*\n)', '', text, flags=re.MULTILINE).split()):
        if s == 'add':
            yield (ADD, None)
        elif s == 'sub':
            yield (SUB, None)
        elif s == 'and':
            yield (AND, None)
        elif s == 'or':
            yield (OR, None)
        elif s == 'eq':
            yield (EQ, None)
        elif s == 'gt':
            yield (GT, None)
        elif s == 'lt':
            yield (LT, None)
        elif s == 'neg':
            yield (NEG, None)
        elif s == 'not':
            yield (NOT, None)
        elif s == 'push':
            yield (PUSH, None)
        elif s == 'pop':
            yield (POP, None)
        elif s == 'label':
            yield (LABEL, None)
        elif s == 'goto':
            yield (GOTO, None)
        elif s == 'if-goto':
            yield (IFGOTO, None)
        elif s == 'function':
            yield (FUNC, None)
        elif s == 'call':
            yield (CALL, None)
        elif s == 'return':
            yield (RET, None)
        elif s in segment_names:
            yield (SEG, s)
        elif re.fullmatch('[0-9]+', s):
            yield (NUM, s)
        elif re.fullmatch('[a-zA-Z_.][a-zA-Z_.0-9]*', s):
            yield (SYM, s)

def parse(text : str):
    lookahead = lambda t: next(t, None)
    lexer = lex(text)
    asm = ""
    tmp = None
    for t,x in lexer:
        if t == ADD:
            # pop 2 from stack and do add
            #
            # @SP
            # A=M-1
            # D=M
            # @SP
            # A=M
            # D=D+M
            # @SP
            # A=M-1
            # M=D
            # D=A
            # @SP
            # M=D
        elif t == SUB:
        elif t == AND:
        elif t == OR:
        elif t == EQ:
        elif t == GT:
        elif t == LT:
        elif t == NEG:
        elif t == NOT:
        elif t == PUSH:
            if (tmp := lookahead(t))[0] != SEG:
                raise Exception('expected segment')
            seg = tmp[1]
            if (tmp := lookahead(t))[0] != num:
                raise Exception('expected number')
            num = tmp[1]
            if seg == 'constant':
                # push constant 13
                #
                # @SP
                # A=M
                # M=13
                # D=A+1
                # @SP
                # M=D
        elif t == POP:
        elif t == LABEL:
        elif t == GOTO:
        elif t == IFGOTO:
        elif t == FUNC:
        elif t == CALL:
        elif t == RET:

with open('MemoryAccess/PointerTest/PointerTest.vm', 'r') as f:
    text = f.read()
    for t in (l := lex(text)):
        print(t)
        print(next(l, None))
