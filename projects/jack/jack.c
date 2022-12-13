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

#define ARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(x) (sizeof(x) / sizeof(*x)) - 1 /* compile-time strlen */

enum tokens {
	T_CLASS = 256,
	T_CONSTRUCT,
	T_FUNC,
	T_METHOD,
	T_FIELD,
	T_STATIC,
	T_VAR,
	T_INT, T_CHAR, T_BOOL, T_VOID,
	T_TRUE, T_FALSE, T_NULL,
	T_THIS,
	T_LET,
	T_DO,
	T_IF, T_ELSE, T_WHILE, T_RETURN,
	T_NUMBER = 300,
	T_STRING,
	T_ID,
	T_END = -1,
	T_ILLEGAL = -2,
};

/* jack grammar
 *
 * class: 'class' className '{' classVarDec* subroutineDec* '}'
 * classVarDec: ('static'|'field') type varName (',' varName)* ';'
 * type: 'int' | 'char' | 'boolean' | className
 * subroutineDec: ('constructor'|'function'|'method') ('void'|type) subroutineName '(' parameterList ')' subroutineBody
 * parameterList: ((type varName) (',' type varName)*)?
 * subroutineBody: '{' varDec* statements '}'
 * varDec: 'var' type varName (',' varName)* ';'
 * className: identifier
 * subroutineName: identifier
 * varName: identifier
 *
 * statements: statement*
 * statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement
 * letStatement: 'let' varName ('[' expression ']')? '=' expression ';'
 * ifStatement: 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
 * whileStatement: 'while' '(' expression ')' '{' statements '}'
 * doStatement: 'do' subroutineCall ';'
 * returnStatement: 'return' expression? ';'
 *
 * expression: term (op term)*
 * term: integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term
 * subroutineCall: subroutineName '(' expressionList ')' | (className|varName) '.' subroutineName '(' expressionList ')'
 * expressionList: (expression (',' expression)*)?
 * op: '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
 * unaryOp: '-' | '~'
 * keywordConstant: 'true' | 'false' | 'null' | 'this'
 */

size_t keyword_length[] = {
	STRLEN("class"),
	STRLEN("constructor"),
	STRLEN("function"),
	STRLEN("method"),
	STRLEN("field"),
	STRLEN("static"),
	STRLEN("var"),
	STRLEN("int"), STRLEN("char"), STRLEN("boolean"), STRLEN("void"),
	STRLEN("true"), STRLEN("false"), STRLEN("null"),
	STRLEN("this"),
	STRLEN("let"),
	STRLEN("do"),
	STRLEN("if"), STRLEN("else"), STRLEN("while"), STRLEN("return"),
};

char *keyword[] = {
	"class",
	"constructor",
	"function",
	"method",
	"field",
	"static",
	"var",
	"int", "char", "boolean", "void",
	"true", "false", "null",
	"this",
	"let",
	"do",
	"if", "else", "while", "return",
};

typedef struct {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	int token;
} Lexer;

/* global variables for parsing */
Lexer lexer;
unsigned int depth;
Fmap fm; /* file map */

void cleanup(void) {
	fmapclose(&fm);
}

int lex(void) {
	char *tp, *s;
	int check;

	if(!lexer.ptr) {
		lexer.ptr = lexer.src;
		char *r = lexer.src;
		char *w = r;

		while(*r) {
			if(r[0] == '/' && r[1] == '/') { /* single-line comments */
				while(*r != '\n') ++r;
				++r;
			} else if(r[0] == '/' && r[1] == '*') { /* multi-line and doc comments */
				if(r[2] == '*')
					r += 3;
				else
					r += 2;
				while(!(r[0] == '*' && r[1] == '/')) ++r;
				r += 2;
			} else if(r != lexer.ptr && isspace(*(r-1)) && isspace(*r)) {
				while(isspace(*r)) ++r; /* delete empty lines */
			} else if(w == lexer.ptr && isspace(*r)) {
				while(isspace(*r)) ++r;
			} else if(isspace(*r)) {
				*w++ = ' ';
				while(isspace(*r)) ++r;
			} else
				*w++ = *r++;
		}
		*w = 0;

		return 0;
	}
	
	while(*lexer.ptr == ' ') ++lexer.ptr;
	tp = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0)
		return T_END;

	lexer.text_s = lexer.text_e = 0;
	lexer.token = 0;

	/* symbol */
	switch(*tp) {
	case '{': case '}': case '(': case ')': case '[': case ']': case '.': case ',': case ';':
	case '+': case '-': case '*': case '/': case '&': case '|': case '<': case '>': case '=': case '~':
		++lexer.ptr;
		return (lexer.token = *tp);
	}

	/* keyword */
	for(int i = 0; i < ARRLEN(keyword); ++i) {
		if(strstr(tp, keyword[i]) == tp) {
			lexer.ptr += keyword_length[i];
			return (lexer.token = T_CLASS + i);
		}
	}

	/* numbers, strings and identifiers */
	for(check = 0, s = tp; *s && isdigit(*s); ++s)
		++check;
	if(check) {
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = T_NUMBER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		if(!check)
			error(1, 0, "mismatched quotes");
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = T_STRING);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = T_ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	lexer.text_s = tp;
	lexer.text_e = lexer.ptr = s;
	return (lexer.token = T_ID);
}

// TODO generate vm code

#define PARSER_PRINT(indent, fmt, ...) { \
	register unsigned int i = indent; \
	while(i > 0) { \
		putc(' ', stderr); \
		putc(' ', stderr); \
		--i; \
	} \
	fprintf(stderr, fmt, ##__VA_ARGS__); \
}

int class(void);
int classVarDec(void);
int varDec(void);
int subroutineDec(void);
void parameterList(void);
void subroutineBody(void);
void statements(void);
int letStatement(void);
int ifStatement(void);
int whileStatement(void);
int doStatement(void);
int returnStatement(void);
void subroutineCall(void);
void expressionList(void);
void expression(void);
int term(void);
void variableDeclaration(void);

/* used in classVarDec and varDec */
void variableDeclaration(void) {
	lex();
	if(lexer.token ==T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
	} else if(lexer.token == T_ID) { /* class type */
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != T_ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
}

int class(void) {
	if(lex() != T_CLASS)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	fprintf(stderr,"<class>\n");
	PARSER_PRINT(depth, "<keyword> class </keyword>\n");

	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	while(classVarDec());

	while(subroutineDec());

	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
	fprintf(stderr,"</class>\n");

	return 1;
}

/* subroutineBody: '{' varDec* statements '}'
 *
 * varDec: 'var' type varName (',' varName)* ';'
 *
 * statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement
 */
void subroutineBody(void) {
	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<subroutineBody>\n");
	++depth;
	PARSER_PRINT(depth, "<symbol> { </symbol>\n");

	while(varDec() == 1);

	statements();
	
	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> } </symbol>\n");
	--depth;
	PARSER_PRINT(depth, "</subroutineBody>\n");
}

int varDec(void) {
	if(lex() != T_VAR) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	PARSER_PRINT(depth, "<varDec>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> var </keyword>\n");

	lex();
	if(lexer.token ==T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
	} else if(lexer.token == T_ID) { /* class type */
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != T_ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	--depth;
	PARSER_PRINT(depth, "</varDec>\n");

	return 1;
}

/* statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement */
void statements(void) {
	PARSER_PRINT(depth, "<statements>\n");
	++depth;
	while(1) {
		if(letStatement()) continue;
		if(ifStatement()) continue;
		if(whileStatement()) continue;
		if(doStatement()) continue;
		if(returnStatement()) continue;

		break;
	}
	--depth;
	PARSER_PRINT(depth, "</statements>\n");
}

int letStatement(void) {
	if(lex() != T_LET) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(depth, "<letStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> let </keyword>\n");

	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	if(lex() != '[') {
		lexer.ptr = lexer.unget;
	} else {
		PARSER_PRINT(depth, "<symbol> [ </symbol>\n");
		expression();
		if(lex() != ']')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> ] </symbol>\n");
	}

	if(lex() != '=')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> = </symbol>\n");

	expression();

	if(lex() != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ; </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</letStatement>\n");
	return 1;
}

int ifStatement(void) {
	if(lex() != T_IF) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(depth, "<ifStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> if </keyword>\n");

	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ( </symbol>\n");

	expression();

	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ) </symbol>\n");

	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> { </symbol>\n");

	statements();

	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> } </symbol>\n");

	if(lex() == T_ELSE) {
		PARSER_PRINT(depth, "<keyword> else </keyword>\n");

		if(lex() != '{')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> { </symbol>\n");

		statements();

		if(lex() != '}')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> } </symbol>\n");
	} else
		lexer.ptr = lexer.unget;

	--depth;
	PARSER_PRINT(depth, "</ifStatement>\n");
	return 1;
}

int whileStatement(void) {
	if(lex() != T_WHILE) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(depth, "<whileStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> while </keyword>\n");

	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ( </symbol>\n");

	expression();

	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ) </symbol>\n");

	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> { </symbol>\n");

	statements();

	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> } </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</whileStatement>\n");
	return 1;
}

int doStatement(void) {
	if(lex() !=T_DO) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(depth, "<doStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> do </keyword>\n");

	subroutineCall();

	if(lex() != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ; </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</doStatement>\n");
	return 1;
}

int returnStatement(void) {
	if(lex() != T_RETURN) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(depth, "<returnStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> return </keyword>\n");

	if(lex() == ';') {
		PARSER_PRINT(depth, "<symbol> ; </symbol>\n");
		--depth;
		PARSER_PRINT(depth, "</returnStatement>\n");
		return 1;
	} else
		lexer.ptr = lexer.unget;

	expression();

	if(lex() != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ; </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</returnStatement>\n");
	return 1;
}

/* expression: term (op term)* */
void expression(void) {
	PARSER_PRINT(depth, "<expression>\n");
	++depth;
	if(term() == 0) {
		--depth;
		PARSER_PRINT(depth, "</expression>\n");
		return;
	}

	while(1) {
		lex();

		if(lexer.token != '+' && lexer.token != '-' &&
				lexer.token != '*' && lexer.token != '/' &&
				lexer.token != '&' && lexer.token != '|' &&
				lexer.token != '<' && lexer.token != '>' && lexer.token != '=') {
			lexer.ptr = lexer.unget;
			lexer.token = 0;
			break;
		}
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		term();
	}

	--depth;
	PARSER_PRINT(depth, "</expression>\n");
}

int term(void) {
	register char *tmp1, *tmp2, *tmp3;

	if(lex() == T_END)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	switch(lexer.token) {
	case T_NUMBER:
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<integerConstant> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </integerConstant>\n", 1, STRLEN(" </integerConstant>\n"), stderr);
		break;
	case T_STRING:
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<stringConstant> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </stringConstant>\n", 1, STRLEN(" </stringConstant>\n"), stderr);
		break;
	case T_TRUE: case T_FALSE: case T_NULL: case T_THIS:
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n",keyword[lexer.token-T_CLASS]);
		break;
	case T_ID:
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		tmp1 = lexer.unget;
		tmp2 = lexer.text_s;
		tmp3 = lexer.text_e;
		lex();

		if(lexer.token == '(' || lexer.token == '.') {
			lexer.ptr = tmp1;
			subroutineCall();
			break;
		}

		PARSER_PRINT(depth, "<identifier> ");
		fwrite(tmp2, 1, tmp3 - tmp2, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

		if(lexer.token != '[') {
			lexer.ptr = lexer.unget;
		} else {
			PARSER_PRINT(depth, "<symbol> [ </symbol>\n");
			expression();
			if(lex() != ']')
				error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
			PARSER_PRINT(depth, "<symbol> ] </symbol>\n");
		}
		break;
	case '(':
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<symbol> ( </symbol>\n");
		expression();
		if(lex() != ')')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> ) </symbol>\n");
		break;
	case '-': case '~':
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
		if(term() != 1)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		break;
	default:
		lexer.token = 0;
		lexer.ptr = lexer.unget;
		return 0;
	}

	--depth;
	PARSER_PRINT(depth, "</term>\n");
	return 1;
}

void subroutineCall(void) {
	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<subroutineCall>\n");
	++depth;
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	if(lex() != '.') {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
	} else {
		PARSER_PRINT(depth, "<symbol> . </symbol>\n");
		if(lex() != T_ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}

	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ( </symbol>\n");
	expressionList();
	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ) </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</subroutineCall>\n");
}

void expressionList(void) {
	PARSER_PRINT(depth, "<expressionList>\n");
	++depth;

	expression();
	while(1) {
		if(lex() != ',') {
			lexer.token = 0;
			lexer.ptr = lexer.unget;
			break;
		}
		PARSER_PRINT(depth, "<symbol> , </symbol>\n");

		expression();
	}

	--depth;
	PARSER_PRINT(depth, "</expressionList>\n");
}

void parameterList(void) {
	/* type */
	if(lex() == T_ID) {
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
	} else if(lexer.token == ')') { /* empty parameter list */
		lexer.ptr = lexer.unget;
		return;
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	/* varName */
	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		/* type */
		if(lex() == T_ID) {
			PARSER_PRINT(depth, "<identifier> ");
			fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
			fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
		} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {
			PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
		} else
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

		/* varName */
		if(lex() != T_ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}
	lexer.ptr = lexer.unget;
}

/*
 * subroutineDec:
  ('constructor'|'function'|'method') ('void'|type) subroutineName '(' parameterList ')' subroutineBody

 * parameterList: ((type varName) (',' type varName)*)?
 *
 */
int subroutineDec(void) {
	lex();
	if(lexer.token != T_CONSTRUCT && lexer.token != T_FUNC && lexer.token != T_METHOD) {
				lexer.ptr = lexer.unget;
		lexer.token = 0;
		return 0;
	}

	PARSER_PRINT(depth, "<subroutineDec>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
	
	/* return type */
	lex();
	if(lexer.token == T_ID) {
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	/* subroutineName */
	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	/* parameterList */
	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
	PARSER_PRINT(depth, "<parameterList>\n");
	++depth;
	parameterList();
	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	--depth;
	PARSER_PRINT(depth, "</parameterList>\n");
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	subroutineBody();

	--depth;
	PARSER_PRINT(depth, "</subroutineDec>\n");

	return 1;
}

/* classVarDec: ('static'|'field') type varName (',' varName)* ';' */
int classVarDec(void) {
	lex();
	if(lexer.token != T_STATIC && lexer.token != T_FIELD) {
				lexer.ptr = lexer.unget;
		lexer.token = 0;
		return 0;
	}

	PARSER_PRINT(depth, "<classVarDec>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

	lex();
	if(lexer.token ==T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);
	} else if(lexer.token == T_ID) { /* class type */
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	if(lex() != T_ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != T_ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	--depth;
	PARSER_PRINT(depth, "</classVarDec>\n");

	return 1;
}

void parse(void) {
	lex();
	class();
}

int main(int argc, char *argv[]) {
	atexit(cleanup);
	fmapopen(argv[1], O_RDONLY, &fm);
	fmapread(&fm);

	lexer.src = fm.buf;
	lexer.ptr = 0;

	depth = 1;
	
	parse();

	fmapclose(&fm);
	return 0;
}
