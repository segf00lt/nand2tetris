#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <error.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fmap.c"

typedef struct {
	int cmd;
	char *arg1, *arg2;
} Snode;

enum TOKENS {
	ADD = 1, SUB, AND, OR,
	NEG, NOT,
	EQ, GT, LT,
	PUSH, POP,
	LABEL, GOTO, IFGOTO,
	FUNC, CALL, RET,
	SEG, ID, NUM,
	END,
	ILLEGAL,
};

char *TOKEN_DBG[] = {
	"",
	"ADD", "SUB", "AND", "OR",
	"NEG", "NOT",
	"EQ", "GT", "LT",
	"PUSH", "POP",
	"LABEL", "GOTO", "IFGOTO",
	"FUNC", "CALL", "RET",
	"SEG", "ID", "NUM",
	"END",
	"ILLEGAL",
};

int lex(char *text, char **curtoken) {
	static char *ptr;
	char *tp, *s;
	int check;
	char *segments[] = {
		"argument",
		"local",
		"this",
		"that",
		"temp",
		"pointer",
		"static",
		"constant",
	};

	if(text) { /* call on text to prime lexer and preprocess, subsequent calls should be on (char*)0 */
		ptr = text;
		char *w = text;

		while(*text) {
			if(text[0] == '/' && text[1] == '/') {
				while(*text != '\n') ++text;
				++text;
			} else if(text != ptr && isspace(*(text-1)) && isspace(*text)) {
				while(isspace(*text)) ++text; /* delete empty lines */
			} else if(isspace(*text)) {
				*w++ = ' ';
				while(isspace(*text)) ++text;
			} else
				*w++ = *text++;
		}
		*w = 0;

		return 0;
	}
	
	for(tp = ptr; *ptr != ' ' && *ptr != 0; ++ptr);

	if(*ptr != 0)
		*ptr++ = 0;
	else {
		*curtoken = 0;
		return END;
	}

	*curtoken = tp;

	if(!strcmp(tp, "add")) return ADD;
	else if(!strcmp(tp, "sub")) return SUB;
	else if(!strcmp(tp, "and")) return AND;
	else if(!strcmp(tp, "or")) return OR;
	else if(!strcmp(tp, "neg")) return NEG;
	else if(!strcmp(tp, "not")) return NOT;
	else if(!strcmp(tp, "eq")) return EQ;
	else if(!strcmp(tp, "gt")) return GT;
        else if(!strcmp(tp, "lt")) return LT;
	else if(!strcmp(tp, "push")) return PUSH;
	else if(!strcmp(tp, "pop")) return POP;
	else if(!strcmp(tp, "label")) return LABEL;
	else if(!strcmp(tp, "goto")) return GOTO;
	else if(!strcmp(tp, "if-goto")) return IFGOTO;
	else if(!strcmp(tp, "function")) return FUNC;
	else if(!strcmp(tp, "call")) return CALL;
	else if(!strcmp(tp, "return")) return RET;

	for(int i = 0; i < (sizeof segments / sizeof *segments); ++i)
		if(!strcmp(tp, segments[i])) return SEG;
	
	for(check = 1, s = tp; *s && check; ++s)
		check = isdigit(*s);
	if(check) return NUM;

	if(!(isalpha(*tp) || *tp == '_')) return ILLEGAL;

	for(check = 1, s = tp; *s && check; ++s)
		check = (isalnum(*s) || *s == '_' || *s == '.');
	if(check) return ID;

	return ILLEGAL;
}

Snode* parse(char *text) {
	Snode *nodearr;
	Snode curnode;
	size_t nodecount, nodemax;
	int t0, t1, t2;
	char *s0, *s1, *s2;

	lex(text, 0);
	nodecount = 0;
	nodearr = malloc((nodemax = 16) * sizeof(Snode));

	while((t0 = lex(0, &s0)) != END) {
		curnode.cmd = t0;
		switch(t0) {
		case ILLEGAL:
			free(nodearr);
			error(1, 0, "illegal token %s", s0);
		case PUSH: case POP:
			if((t1 = lex(0, &s1)) != SEG) {
				free(nodearr);
				error(1, 0, "expected segment as 1st arg to %s", s0);
			}
			if((t2 = lex(0, &s2)) != NUM) {
				free(nodearr);
				error(1, 0, "expected number as 2nd arg to %s", s0);
			}
			curnode = (Snode){ .cmd = t0, .arg1 = s1, .arg2 = s2 };
			break;
		case FUNC: case CALL:
			if((t1 = lex(0, &s1)) != ID) {
				free(nodearr);
				error(1, 0, "expected identifier as 1st arg to %s", s0);
			}
			if((t2 = lex(0, &s2)) != NUM) {
				free(nodearr);
				error(1, 0, "expected number as 2nd arg to %s", s0);
			}
			curnode = (Snode){ .cmd = t0, .arg1 = s1, .arg2 = s2 };
			break;
		case LABEL: case GOTO: case IFGOTO:
			if((t1 = lex(0, &s1)) != ID) {
				free(nodearr);
				error(1, 0, "expected identifier as 1st arg to %s", s0);
			}
			curnode = (Snode){ .cmd = t0, .arg1 = s1 };
			break;
		}

		nodearr[nodecount++] = curnode;
		if(nodecount >= nodemax) {
			nodemax <<= 1;
			nodearr = realloc(nodearr, nodemax * sizeof(Snode));
		}
	}

	nodearr[nodecount] = (Snode){0};
	nodearr = realloc(nodearr, (nodecount + 1) * sizeof(Snode)); /* resize to fit */

	return nodearr;
}

void debug_parsed(Snode *nodes) {
	for(int i = 1; nodes->cmd != 0; ++i, ++nodes) {
		fprintf(stderr, "Snode %i:\n\tcmd: %s\n\targ1: %s\n\targ2: %s\n", i,
				TOKEN_DBG[nodes->cmd], nodes->arg1 ? nodes->arg1 : "",
				nodes->arg2 ? nodes->arg2 : "");
	}
}


void generate(Snode *nodes) {
	unsigned int compcount = 0;
	char c;
	char *s;
	char binoptab[] = {'+', '-', '&', '|'};
	char unoptab[] = {'-', '!'};
	char *cmpoptab[] = {"JEQ", "JGT", "JLT"};

	for(Snode cur = *nodes; cur.cmd; cur = *++nodes) {
		switch(cur.cmd) {
		case ADD: case SUB: case AND: case OR:
			c = binoptab[cur.cmd - ADD];
			printf("@SP\nA=M-1\nD=M\nA=A-1\nM=M%cD\nD=A+1\n@SP\nM=D\n", c);
			break;
		case NEG: case NOT:
			c = unoptab[cur.cmd - NEG];
			printf("@SP\nA=M-1\nM=%cM\n", c);
			break;
		case EQ: case GT: case LT:
			s = cmpoptab[cur.cmd - EQ];
			printf("@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\nM=-1\n@SP\nM=M-1\n");

			/* TODO optimize chained comparison? */
			/* common case optimization */
			if(nodes[1].cmd == IFGOTO) {
				++nodes;
				printf("@%s\nD;%s\n@SP\nA=M-1\nM=0\n", nodes->arg1, s);
				break;
			}
			
			printf("@TRUE_COMPARISON_%i\nD;%s\n@SP\nA=M-1\nM=0\n(TRUE_COMPARISON_%i)\n",
					compcount, s, compcount);
			++compcount;
			break;
		case PUSH:
			if(!strcmp(cur.arg1, "constant")) {
				printf("@%s\nD=A\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "argument")) {
				printf("@%s\nD=A\n@ARG\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "local")) {
				printf("@%s\nD=A\n@LCL\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "this")) {
				printf("@%s\nD=A\n@THIS\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "that")) {
				printf("@%s\nD=A\n@THAT\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "temp")) {
				/* R5-12 */
				printf("@%s\nD=A\n@R5\nA=A+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "pointer")) {
				/* R13-14 */
				printf("@%s\nD=A\n@R13\nA=A+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "static")) {
				/* TODO is the static segment R16 onward? */
				printf("@%s\nD=A\n@R15\nA=M+D\nD=M\n", cur.arg2);
			}
			printf("@SP\nA=M\nM=D\n@SP\nM=M+1\n");
			break;
		case POP:
			break;
		}
	}
}

int main(int argc, char *argv[]) {
	Fmap fm;
	fmapopen(argv[1], O_RDONLY, &fm);
	fmapread(&fm);
	Snode *syntaxarr = parse(fm.buf);
	debug_parsed(syntaxarr);
	generate(syntaxarr);
	free(syntaxarr);
	fmapclose(&fm);
	return 0;
}