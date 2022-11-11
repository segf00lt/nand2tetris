#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <error.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <libgen.h>
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

	if(!(isalpha(*tp) || *tp == '_' || *tp == '.' || *tp == '$' || *tp == ':')) return ILLEGAL;

	for(check = 1, s = tp; *s && check; ++s)
		check = (isalnum(*s) || *s == '_' || *s == '.' || *tp == '$' || *tp == ':');
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
		case LABEL: case GOTO: case IFGOTO:
			if((t1 = lex(0, &s1)) != ID) {
				free(nodearr);
				error(1, 0, "expected identifier as 1st arg to %s", s0);
			}
			curnode = (Snode){ .cmd = t0, .arg1 = s1 };
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
				TOKEN_DBG[nodes->cmd-1], nodes->arg1 ? nodes->arg1 : "",
				nodes->arg2 ? nodes->arg2 : "");
	}
}

void init(char *destname, FILE *dest) {
	fprintf(dest,
			"@256\nD=A\n@SP\nM=D\n"
			"@%s.RET_0\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
			"@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
			"@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
			"@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
			"@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
			"@SP\nD=M\n@5\nD=D-A\n@ARG\nM=D\n" // is this necessary?
			"@SP\nD=M\n@LCL\nM=D\n"
			"@Sys.init\n0;JMP\n"
			"(%s.RET_0)\n",
			destname, destname);
}

void compile(char *fname, char *text, FILE *dest) {
	char c;
	char *s;
	char curfunc[128] = "null";
	char binoptab[] = {'+', '-', '&', '|'};
	char unoptab[] = {'-', '!'};
	char *cmpoptab[] = {"JEQ", "JGT", "JLT"};
	unsigned int compcount = 0;
	unsigned int callcount = 1;
	Snode *syntaxarr = parse(text);
	Snode *nodes = syntaxarr;
	debug_parsed(syntaxarr);

	for(Snode cur = *nodes; cur.cmd; cur = *++nodes) {
		switch(cur.cmd) {
		case ADD: case SUB: case AND: case OR:
			c = binoptab[cur.cmd - ADD];
			fprintf(dest, "@SP\nA=M-1\nD=M\nA=A-1\nM=M%cD\nD=A+1\n@SP\nM=D\n", c);
			break;
		case NEG: case NOT:
			c = unoptab[cur.cmd - NEG];
			fprintf(dest, "@SP\nA=M-1\nM=%cM\n", c);
			break;
		case EQ: case GT: case LT:
			s = cmpoptab[cur.cmd - EQ];
			fprintf(dest, "@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\nM=-1\n@SP\nM=M-1\n");

			fprintf(dest, "@%s.TRUE_COMPARISON_%i\nD;%s\n@SP\nA=M-1\nM=0\n(%s.TRUE_COMPARISON_%i)\n",
					fname, compcount, s, fname, compcount);
			++compcount;
			break;
		case PUSH:
			if(!strcmp(cur.arg1, "constant")) {
				fprintf(dest, "@%s\nD=A\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "argument")) {
				fprintf(dest, "@%s\nD=A\n@ARG\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "local")) {
				fprintf(dest, "@%s\nD=A\n@LCL\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "this")) {
				fprintf(dest, "@%s\nD=A\n@THIS\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "that")) {
				fprintf(dest, "@%s\nD=A\n@THAT\nA=M+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "temp")) {
				/* R5-12 */
				fprintf(dest, "@%s\nD=A\n@R5\nA=A+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "pointer")) {
				/* R13-14 */
				fprintf(dest, "@%s\nD=A\n@THIS\nA=A+D\nD=M\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "static")) {
				fprintf(dest, "@%s.%s\nD=M\n", fname, cur.arg2);
			}

			fprintf(dest, "@SP\nA=M\nM=D\n@SP\nM=M+1\n");
			break;
		case POP:
			/* pop uses R15 to store destination address */
			if(!strcmp(cur.arg1, "constant")) {
				fprintf(dest, "@R15\nM=A\n");
			} else if(!strcmp(cur.arg1, "argument")) {
				fprintf(dest, "@%s\nD=A\n@ARG\nD=M+D\n@R15\nM=D\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "local")) {
				fprintf(dest, "@%s\nD=A\n@LCL\nD=M+D\n@R15\nM=D\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "this")) {
				fprintf(dest, "@%s\nD=A\n@THIS\nD=M+D\n@R15\nM=D\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "that")) {
				fprintf(dest, "@%s\nD=A\n@THAT\nD=M+D\n@R15\nM=D\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "temp")) {
				/* R5-12 */
				fprintf(dest, "@%s\nD=A\n@R5\nD=A+D\n@R15\nM=D\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "pointer")) {
				/* address of R3 and R4 */
				fprintf(dest, "@%s\nD=A\n@THIS\nD=A+D\n@R15\nM=D\n", cur.arg2);
			} else if(!strcmp(cur.arg1, "static")) {
				fprintf(dest, "@%s.%s\nD=A\n@R15\nM=D\n", fname, cur.arg2);
			}

			fprintf(dest, "@SP\nM=M-1\nA=M\nD=M\n@R15\nA=M\nM=D\n");
			break;
		case LABEL:
			fprintf(dest, "(%s$%s)\n", curfunc, cur.arg1);
			break;
		case GOTO:
			fprintf(dest, "@%s$%s\n0;JMP\n", curfunc, cur.arg1);
			break;
		case IFGOTO:
			fprintf(dest, "@SP\nM=M-1\nA=M\nD=M\n@%s$%s\nD;JNE\n", curfunc, cur.arg1);
			break;
		case FUNC:
			/* nested function declarations are not allowed */
			sprintf(curfunc, "%s", cur.arg1);
			fprintf(dest, "(%s)\n@0\nD=A\n", curfunc);
			for(int k = atoi(cur.arg2), i = 0; i < k; ++i)
				fprintf(dest, "@SP\nA=M\nM=D\n@SP\nM=M+1\n");
			break;
		case CALL:
			fprintf(dest,
					"@%s.RET_%i\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
					"@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
					"@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
					"@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
					"@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
					"@SP\nD=M\n@%s\nD=D-A\n@5\nD=D-A\n@ARG\nM=D\n"
					"@SP\nD=M\n@LCL\nM=D\n"
					"@%s\n0;JMP\n"
					"(%s.RET_%i)\n",
					fname, callcount, cur.arg2, cur.arg1, fname, callcount);
			++callcount;
			break;
		case RET:
			fprintf(dest,
					"@LCL\nD=M\n@R5\nM=D\n"
					"@5\nA=D-A\nD=M\n@R6\nM=D\n"
					"@SP\nA=M-1\nD=M\n@ARG\nA=M\nM=D\n"
					"@ARG\nD=M+1\n@SP\nM=D\n"
					"@R5\nM=M-1\nA=M\nD=M\n@THAT\nM=D\n"
					"@R5\nM=M-1\nA=M\nD=M\n@THIS\nM=D\n"
					"@R5\nM=M-1\nA=M\nD=M\n@ARG\nM=D\n"
					"@R5\nM=M-1\nA=M\nD=M\n@LCL\nM=D\n"
					"@R6\nA=M\n0;JMP\n");
			break;
		}
	}

	free(syntaxarr);
}

int main(int argc, char *argv[]) {
	Fmap fm;
	char path[256];
	char *base;
	char *ptr;
	FILE *dest;
	DIR *dp;
	struct dirent *ent;
	struct stat status;
	int fd = open(argv[1], O_RDONLY);
	fstat(fd, &status);
	dest = fopen(argv[2], "w");
	char srcname[128];
	char destname[128];

	strcpy(srcname, basename(argv[1]));
	if((ptr = strstr(srcname, ".vm")))
		*ptr = 0;
	strcpy(destname, basename(argv[2]));
	if((ptr = strstr(destname, ".asm")))
		*ptr = 0;

	init(destname, dest);

	if(!S_ISDIR(status.st_mode)) {
		fmapfdopen(fd, &fm);
		fmapread(&fm);
		compile(srcname, fm.buf, dest);
		fmapclose(&fm);
		fclose(dest);
		return 0;
	}

	dp = fdopendir(fd);
	strcpy(path, argv[1]);
	if(!strchr(path, '/'))
		strcat(path, "/");
	base = path + strlen(path);

	while((ent = readdir(dp)) != 0) {
		strcpy(base, ent->d_name);
		strcpy(srcname, ent->d_name);
		if(!(ptr = strstr(srcname, ".vm")))
			continue;
		*ptr = 0;
		fmapopen(path, O_RDONLY, &fm);
		fmapread(&fm);
		compile(srcname, fm.buf, dest);
		fmapclose(&fm);
	}

	fclose(dest);
	closedir(dp);
	return 0;
}
