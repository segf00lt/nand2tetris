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

enum token_types {
	END = -1,
	KEYWORD,
	SYMBOL,
	NUMBER,
	STRING,
	ID,
	ILLEGAL,
};

enum tokens {
	CLASS = 256,
	CONSTRUCT,
	FUNC,
	METHOD,
	FIELD,
	STATIC,
	VAR,
	INT, CHAR, BOOL, VOID,
	TRUE, FALSE, NUL,
	THIS,
	LET,
	DO,
	IF, ELSE, WHILE, RETURN,
};

size_t keyword_length[] = {
	STRLEN("class"),
	STRLEN("constructor"),
	STRLEN("function"),
	STRLEN("method"),
	STRLEN("field"),
	STRLEN("static"),
	STRLEN("var"),
	STRLEN("int"), STRLEN("char"), STRLEN("boolean"), STRLEN("void"),
	STRLEN("true"), STRLEN("false"), STRLEN("nul"),
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

typedef struct syntax_node {
	int type;
	char *text;
	struct syntax_node *left;
	struct syntax_node *right;
} AST_node;

typedef struct {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	int token;
} Lexer;

typedef struct {
	AST_node *ast;
	AST_node *free;
	size_t size;
	size_t count;
	unsigned int depth;
} Parser;

/* global variables for parsing */
Lexer lexer;
Parser parser;
Fmap fm; /* file map */

AST_node* alloc_node(int type, char *text, AST_node *left, AST_node *right) {
	if(parser.count >= parser.size) {
		register AST_node *new, *old, *np;
		register size_t i;

		parser.size <<= 1;
		old = parser.free;
		new = realloc(parser.free, sizeof(AST_node) * parser.size);

		for(i = 0; i < parser.count; ++i) {
			np = new + i;
			np->left = new + (np->left - old);
			np->right = new + (np->right - old);
		}
	}

	return parser.free + (parser.count++);
}

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
		return END;

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
			return (lexer.token = CLASS + i);
		}
	}

	if(lexer.token)
		return KEYWORD;

	/* numbers, strings and identifiers */
	for(check = 0, s = tp; *s && isdigit(*s); ++s)
		++check;
	if(check) {
		lexer.text_s = tp;
		lexer.text_e = lexer.ptr = s;
		return (lexer.token = NUMBER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		if(!check)
			error(1, 0, "mismatched quotes");
		lexer.text_s = tp + 1;
		lexer.text_e = s;
		lexer.ptr = s + 1;
		return (lexer.token = STRING);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	lexer.text_s = tp;
	lexer.text_e = lexer.ptr = s;
	return (lexer.token = ID);
}

/* TODO
 * 
 * generate AST!!!!!!!
 *
 * cleanup (I dunno figure it out)
 */

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
	if(lexer.token == INT || lexer.token == CHAR || lexer.token == BOOL) { /* builtin type */
		PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
	} else if(lexer.token == ID) { /* class type */
		PARSER_PRINT(parser.depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	if(lex() != ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	while(lex() == ',') {
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);
}

int class(void) {
	if(lex() != CLASS)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	fprintf(stderr,"<class>\n");
	PARSER_PRINT(parser.depth, "<keyword> class </keyword>\n");

	if(lex() != ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

	while(classVarDec());

	while(subroutineDec());

	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);
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
	PARSER_PRINT(parser.depth, "<subroutineBody>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<symbol> { </symbol>\n");

	while(varDec() == 1);

	statements();
	
	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> } </symbol>\n");
	--parser.depth;
	PARSER_PRINT(parser.depth, "</subroutineBody>\n");
}

int varDec(void) {
	if(lex() != VAR) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	PARSER_PRINT(parser.depth, "<varDec>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> var </keyword>\n");

	variableDeclaration();

	--parser.depth;
	PARSER_PRINT(parser.depth, "</varDec>\n");

	return 1;
}

/* statement: letStatement | ifStatement | whileStatement | doStatement | returnStatement */
void statements(void) {
	PARSER_PRINT(parser.depth, "<statements>\n");
	++parser.depth;
	while(1) {
		if(letStatement()) continue;
		if(ifStatement()) continue;
		if(whileStatement()) continue;
		if(doStatement()) continue;
		if(returnStatement()) continue;

		break;
	}
	--parser.depth;
	PARSER_PRINT(parser.depth, "</statements>\n");
}

int letStatement(void) {
	if(lex() != LET) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<letStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> let </keyword>\n");

	if(lex() != ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	if(lex() != '[') {
		lexer.ptr = lexer.unget;
	} else {
		PARSER_PRINT(parser.depth, "<symbol> [ </symbol>\n");
		expression();
		if(lex() != ']')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> ] </symbol>\n");
	}

	if(lex() != '=')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> = </symbol>\n");

	expression();

	if(lex() != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ; </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</letStatement>\n");
	return 1;
}

int ifStatement(void) {
	if(lex() != IF) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<ifStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> if </keyword>\n");

	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ( </symbol>\n");

	expression();

	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ) </symbol>\n");

	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> { </symbol>\n");

	statements();

	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> } </symbol>\n");

	if(lex() == ELSE) {
		PARSER_PRINT(parser.depth, "<keyword> else </keyword>\n");

		if(lex() != '{')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> { </symbol>\n");

		statements();

		if(lex() != '}')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> } </symbol>\n");
	} else
		lexer.ptr = lexer.unget;

	--parser.depth;
	PARSER_PRINT(parser.depth, "</ifStatement>\n");
	return 1;
}

int whileStatement(void) {
	if(lex() != WHILE) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<whileStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> while </keyword>\n");

	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ( </symbol>\n");

	expression();

	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ) </symbol>\n");

	if(lex() != '{')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> { </symbol>\n");

	statements();

	if(lex() != '}')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> } </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</whileStatement>\n");
	return 1;
}

int doStatement(void) {
	if(lex() != DO) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<doStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> do </keyword>\n");

	subroutineCall();

	if(lex() != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ; </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</doStatement>\n");
	return 1;
}

int returnStatement(void) {
	if(lex() != RETURN) {
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<returnStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> return </keyword>\n");

	if(lex() == ';') {
		PARSER_PRINT(parser.depth, "<symbol> ; </symbol>\n");
		--parser.depth;
		PARSER_PRINT(parser.depth, "</returnStatement>\n");
		return 1;
	} else
		lexer.ptr = lexer.unget;

	expression();

	if(lex() != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ; </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</returnStatement>\n");
	return 1;
}

/* expression: term (op term)* */
void expression(void) {
	PARSER_PRINT(parser.depth, "<expression>\n");
	++parser.depth;
	if(term() == 0) {
		--parser.depth;
		PARSER_PRINT(parser.depth, "</expression>\n");
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
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

		term();
	}

	--parser.depth;
	PARSER_PRINT(parser.depth, "</expression>\n");
}

int term(void) {
	register char *tmp1, *tmp2, *tmp3;

	if(lex() == END)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	switch(lexer.token) {
	case NUMBER:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<integerConstant> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </integerConstant>\n", 1, STRLEN(" </integerConstant>\n"), stderr);
		break;
	case STRING:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<stringConstant> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </stringConstant>\n", 1, STRLEN(" </stringConstant>\n"), stderr);
		break;
	case TRUE: case FALSE: case NUL: case THIS:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n",keyword[lexer.token-CLASS]);
		break;
	case ID:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		tmp1 = lexer.unget;
		tmp2 = lexer.text_s;
		tmp3 = lexer.text_e;
		lex();

		if(lexer.token == '(' || lexer.token == '.') {
			lexer.ptr = tmp1;
			subroutineCall();
			break;
		}

		PARSER_PRINT(parser.depth, "<identifier> ");
		fwrite(tmp2, 1, tmp3 - tmp2, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

		if(lexer.token != '[') {
			lexer.ptr = lexer.unget;
		} else {
			PARSER_PRINT(parser.depth, "<symbol> [ </symbol>\n");
			expression();
			if(lex() != ']')
				error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
			PARSER_PRINT(parser.depth, "<symbol> ] </symbol>\n");
		}
		break;
	case '(':
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<symbol> ( </symbol>\n");
		expression();
		if(lex() != ')')
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> ) </symbol>\n");
		break;
	case '-': case '~':
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);
		if(term() != 1)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		break;
	default:
		lexer.token = 0;
		lexer.ptr = lexer.unget;
		return 0;
	}

	--parser.depth;
	PARSER_PRINT(parser.depth, "</term>\n");
	return 1;
}

void subroutineCall(void) {
	if(lex() != ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<subroutineCall>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	if(lex() != '.') {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
	} else {
		PARSER_PRINT(parser.depth, "<symbol> . </symbol>\n");
		if(lex() != ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	}

	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ( </symbol>\n");
	expressionList();
	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ) </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</subroutineCall>\n");
}

void expressionList(void) {
	PARSER_PRINT(parser.depth, "<expressionList>\n");
	++parser.depth;

	expression();
	while(1) {
		if(lex() != ',') {
			lexer.token = 0;
			lexer.ptr = lexer.unget;
			break;
		}
		PARSER_PRINT(parser.depth, "<symbol> , </symbol>\n");

		expression();
	}

	--parser.depth;
	PARSER_PRINT(parser.depth, "</expressionList>\n");
}

void parameterList(void) {
	/* type */
	if(lex() == ID) {
		PARSER_PRINT(parser.depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else if(lexer.token >= INT && lexer.token <= VOID) {
		PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
	} else if(lexer.token == ')') { /* empty parameter list */
		lexer.ptr = lexer.unget;
		return;
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	/* varName */
	if(lex() != ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	while(lex() == ',') {
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

		/* type */
		if(lex() == ID) {
			PARSER_PRINT(parser.depth, "<identifier> ");
			fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
			fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
		} else if(lexer.token >= INT && lexer.token <= VOID) {
			PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
		} else
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

		/* varName */
		if(lex() != ID)
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<identifier> ");
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
	if(lexer.token != CONSTRUCT && lexer.token != FUNC && lexer.token != METHOD) {
				lexer.ptr = lexer.unget;
		lexer.token = 0;
		return 0;
	}

	PARSER_PRINT(parser.depth, "<subroutineDec>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
	
	/* return type */
	lex();
	if(lexer.token == ID) {
		PARSER_PRINT(parser.depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);
	} else if(lexer.token >= INT && lexer.token <= VOID) {
		PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	/* subroutineName */
	if(lex() != ID)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, stderr);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), stderr);

	/* parameterList */
	if(lex() != '(')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token); /* open paren */
	PARSER_PRINT(parser.depth, "<parameterList>\n");
	++parser.depth;
	parameterList();
	if(lex() != ')')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	--parser.depth;
	PARSER_PRINT(parser.depth, "</parameterList>\n");
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

	subroutineBody();

	--parser.depth;
	PARSER_PRINT(parser.depth, "</subroutineDec>\n");

	return 1;
}

/* classVarDec: ('static'|'field') type varName (',' varName)* ';' */
int classVarDec(void) {
	lex();
	if(lexer.token != STATIC && lexer.token != FIELD) {
				lexer.ptr = lexer.unget;
		lexer.token = 0;
		return 0;
	}

	PARSER_PRINT(parser.depth, "<classVarDec>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);

	variableDeclaration();

	--parser.depth;
	PARSER_PRINT(parser.depth, "</classVarDec>\n");

	return 1;
}

void parse(void) {
	/* initialize syntax tree */
	//parser.count = 0;
	//parser.size = 128;
	//parser.free = malloc(sizeof(AST_node) * 128);

	lex();
	class();
	//while(lexer.token != END)
	//	class();
}

void debug_parse(AST_node *ast) {
}

int main(int argc, char *argv[]) {
	atexit(cleanup);
	fmapopen(argv[1], O_RDONLY, &fm);
	fmapread(&fm);
	lexer.src = fm.buf;
	lexer.ptr = 0;
	parser.depth = 1;
	
	parse();

	fmapclose(&fm);
	return 0;
}
