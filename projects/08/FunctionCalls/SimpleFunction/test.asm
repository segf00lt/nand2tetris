(SimpleFunction.vm.SimpleFunction.test)
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
M=D
@SP
M=M+1
@0
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
@1
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
M=M+D
D=A+1
@SP
M=D
@SP
A=M-1
M=!M
@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
M=M+D
D=A+1
@SP
M=D
@1
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
M=M-D
D=A+1
@SP
M=D
@LCL
D=M
@R5
M=D
@5
A=D-A
D=M
@R6
M=D
@SP
A=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@R5
M=M-1
D=M
@THAT
M=D
@R5
M=M-1
D=M
@THIS
M=D
@R5
M=M-1
D=M
@ARG
M=D
@R5
M=M-1
D=M
@LCL
M=D
@R6
A=M
0;JMP
