@color
M=0

(L1)

@KBD
D=M

@UPDATE
D;JEQ
D=-1

(UPDATE)
@key
M=D
@color
D=D-M

@L1
D;JEQ // if color was same as last time skip redraw

@key
D=M
@color
M=D

@SCREEN
D=A
@8192
D=D+A
@p
M=D // p=SCREEN+8192

(L2)
@p
D=M-1
M=D

@L1
D;JLT

@color
D=M
@p
A=M
M=D

@L2
0;JMP
