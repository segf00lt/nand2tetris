#!/usr/bin/env python3

from sys import argv
import re

term = "[01DAM]"
label = "[a-zA-Z0-9_.$:]+"
dest = "((A|M|D|(AMD)|(AM)|(MD)|(AD))=)?"
jump = "(;((JGT)|(JEQ)|(JGE)|(JLT)|(JNE)|(JLE)|(JMP)))?"
comp = f"(({term})|([-!]{term})|({term}[-+&|]{term}))"
Linst = re.compile(f"\({label}\)")
Ainst = re.compile('@' + label)
Cinst = re.compile(dest + comp + jump)

symtab = {
        'SP': 0, 'LCL': 1, 'ARG': 2, 'THIS': 3, 'THAT': 4,
        'R0': 0, 'R1': 1, 'R2': 2, 'R3': 3, 'R4': 4,
        'R5': 5, 'R6': 6, 'R7': 7, 'R8': 8, 'R9': 9,
        'R10': 10, 'R11': 11, 'R12': 12, 'R13': 13, 'R14': 14, 'R15': 15,
        'SCREEN': 0x4000, 'KBD': 0x6000,
        }
desttab = {'A': 0b100, 'D': 0b010, 'M': 0b001}
jumptab = {
        '': 0b000,
        'JGT': 0b001,
        'JEQ': 0b010,
        'JGE': 0b011,
        'JLT': 0b100,
        'JNE': 0b101,
        'JLE': 0b110,
        'JMP': 0b111,
        }
comptab = {
        '0':   0b0101010,
        '1':   0b0111111,
        '-1':  0b0111010,
        'D':   0b0001100,
        'A':   0b0110000,
        'M':   0b1110000,
        '!D':  0b0001101,
        '!A':  0b0110001,
        '!M':  0b1110001,
        '-D':  0b0001111,
        '-A':  0b0110011,
        '-M':  0b1110011,
        'D+1': 0b0011111,
        'A+1': 0b0110111,
        'M+1': 0b1110111,
        'D-1': 0b0001110,
        'A-1': 0b0110010,
        'M-1': 0b1110010,
        'D+A': 0b0000010,
        'D+M': 0b1000010,
        'A+D': 0b0000010,
        'M+D': 0b1000010,
        'D-A': 0b0010011,
        'A-D': 0b0000111,
        'D-M': 0b1010011,
        'M-D': 0b1000111,
        'D&A': 0b0000000,
        'A&D': 0b0000000,
        'D&M': 0b1000000,
        'M&D': 0b1000000,
        'D|A': 0b0010101,
        'A|D': 0b0010101,
        'D|M': 0b1010101,
        'M|D': 0b1010101,
        }

def assemble(asm : str) -> str:
    prog = ""
    src = []
    memcount = 0x10
    labelcount = 0
    for i,x in enumerate(asm.split()):
        if Linst.fullmatch(x):
            symtab[x[1:-1]] = i - labelcount
            labelcount += 1
        else:
            src += [x]
    for i,x in enumerate(src):
        inst = ""
        if Ainst.fullmatch(x):
            arg = x[1:]
            value = 0
            if arg.isdigit():
                value = int(arg)
            else:
                try:
                    value = symtab[arg]
                except KeyError:
                    value = memcount
                    symtab[arg] = value
                    memcount += 1
            inst = '0{:015b}\n'.format(value)
        elif Cinst.fullmatch(x):
            a = -1
            b = None
            dstr = cstr = jstr = ''
            dfield = cfield = jfield = 0
            if '=' in x:
                a = x.find('=')
                dstr = x[:a]
                for reg in dstr:
                    dfield |= desttab[reg]
            if ';' in x:
                b = x.find(';')
                jstr = x[b+1:]
                jfield = jumptab[jstr]
            if dstr == '' and jump == '':
                raise Exception(f"line {i}: missing dest or jump")
            cstr = x[a+1:b]
            try:
                cfield = comptab[cstr]
            except KeyError:
                raise Exception(f"line {i}: bad expression {cstr}")
            inst = '111{:013b}\n'.format((cfield << 6) | (dfield << 3) | jfield)
        else:
            raise Exception(f"line {i}: unkown instruction {x}")
        prog += inst
    return prog

with open(argv[1], 'r') as f:
    asm = re.sub('(^\n)|(//.*\n)', '', f.read(), flags=re.MULTILINE)

aout = assemble(asm)

with open(argv[1].replace('asm', 'hack'), 'w') as f:
    f.write(aout)
