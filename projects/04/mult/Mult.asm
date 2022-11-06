// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
//
// This program only needs to handle arguments that satisfy
// R0 >= 0, R1 >= 0, and R0*R1 < 32768.

// Put your code here.
@2
M=0

@acc
M=0

// use smaller argument as counter
@0
D=M
@1
D=D-M // R0 - R1
@CNTR1
D;JGT

// if R0 <= R1
@0
D=M
@END // jump to end if R0 == 0
D;JEQ
@cnt
M=D

@1
D=M
@inc
M=D

@LOOP
0;JMP

(CNTR1) // if R1 < R0
@1
D=M
@END // jump to end if R1 == 0
D;JEQ
@cnt
M=D

@0
D=M
@inc
M=D

(LOOP)
@acc
D=M

@inc
D=D+M
@acc
M=D

@cnt
M=M-1
D=M

@LOOP
D;JGT

(END)
@acc
D=M
@2
M=D
