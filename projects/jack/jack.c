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
	char *text;
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
	if(lexer.text)
		free(lexer.text);
	fmapclose(&fm);
}

int lex(void) {
	char *tp, *s;
	int check;
	int tmp;

	if(!lexer.ptr) {
		lexer.ptr = lexer.src;
		char *s = lexer.src;
		char *w = s;

		while(*s) {
			if(s[0] == '/' && s[1] == '/') { /* single-line comments */
				while(*s != '\n') ++s;
				++s;
			} else if(s[0] == '/' && s[1] == '*') { /* multi-line and doc comments */
				if(s[2] == '*')
					s += 3;
				else
					s += 2;
				while(!(s[0] == '*' && s[1] == '/')) ++s;
				s += 2;
			} else if(s != lexer.ptr && isspace(*(s-1)) && isspace(*s)) {
				while(isspace(*s)) ++s; /* delete empty lines */
			} else if(w == lexer.ptr && isspace(*s)) {
				while(isspace(*s)) ++s;
			} else if(isspace(*s)) {
				*w++ = ' ';
				while(isspace(*s)) ++s;
			} else
				*w++ = *s++;
		}
		*w = 0;

		return 0;
	}
	
	while(*lexer.ptr == ' ') ++lexer.ptr;
	tp = lexer.unget = lexer.ptr;

	if(*lexer.ptr == 0)
		return END;

	lexer.text = 0;
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
		tmp = *s;
		*s = 0;
		lexer.text = strdup(tp);
		*s = tmp;
		lexer.ptr = s;
		return (lexer.token = NUMBER);
	}

	if(*tp == '"') {
		for(s = tp + 1; (check = *s) && *s != '"'; ++s);
		if(!check)
			error(1, 0, "mismatched quotes");
		*s = 0;
		lexer.text = strdup(tp + 1);
		*s = '"';
		lexer.ptr = s + 1;
		return (lexer.token = STRING);
	}

	if(!(isalpha(*tp) || *tp == '_')) return (lexer.token = ILLEGAL);

	for(s = tp; *s && (isalnum(*s) || *s == '_'); ++s);

	tmp = *s;
	*s = 0;
	lexer.text = strdup(tp);
	*s = tmp;
	lexer.ptr = s;
	return (lexer.token = ID);
}

/* TODO
 * write parser
 *
 * refactor ID, NUMBER and STRING alloc
 *
 * NOTE allocate sybmols in an arena
 * maybe store them in the symbol table immediately
 * and avoid allocating the same string twice?
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
		putchar(' '); \
		putchar(' '); \
		--i; \
	} \
	printf(fmt, ##__VA_ARGS__); \
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
		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
		free(lexer.text);
	} else
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);

	if(lex() != ID)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
	free(lexer.text);

	while(lex() == ',') {
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != ID)
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
		free(lexer.text);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);
}

int class(void) {
	if(lex() != CLASS)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);

	printf("<class>\n");
	PARSER_PRINT(parser.depth, "<keyword> class </keyword>\n");

	if(lex() != ID)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
	free(lexer.text);

	if(lex() != '{')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

	while(classVarDec());

	while(subroutineDec());

	if(lex() != '}')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);
	printf("</class>\n");

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
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<subroutineBody>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<symbol> { </symbol>\n");

	while(varDec() == 1);

	statements();
	
	if(lex() != '}')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> } </symbol>\n");
	--parser.depth;
	PARSER_PRINT(parser.depth, "</subroutineBody>\n");
}

int varDec(void) {
	if(lex() != VAR) {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
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
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<letStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> let </keyword>\n");

	if(lex() != ID)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
	free(lexer.text);

	if(lex() != '[') {
		lexer.ptr = lexer.unget;
	} else {
		PARSER_PRINT(parser.depth, "<symbol> '[' </symbol>\n");
		expression();
		if(lex() != ']')
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> ']' </symbol>\n");
	}

	if(lex() != '=')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '=' </symbol>\n");

	expression();

	if(lex() != ';')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ';' </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</letStatement>\n");
	return 1;
}

int ifStatement(void) {
	if(lex() != IF) {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<ifStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> if </keyword>\n");

	if(lex() != '(')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '(' </symbol>\n");

	expression();

	if(lex() != ')')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ')' </symbol>\n");

	if(lex() != '{')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '{' </symbol>\n");

	statements();

	if(lex() != '}')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '}' </symbol>\n");

	if(lex() == ELSE) {
		PARSER_PRINT(parser.depth, "<keyword> else </keyword>\n");

		if(lex() != '{')
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> '{' </symbol>\n");

		statements();

		if(lex() != '}')
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> '}' </symbol>\n");
	} else {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
	}

	--parser.depth;
	PARSER_PRINT(parser.depth, "</ifStatement>\n");
	return 1;
}

int whileStatement(void) {
	if(lex() != WHILE) {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<whileStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> while </keyword>\n");

	if(lex() != '(')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '(' </symbol>\n");

	expression();

	if(lex() != ')')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ')' </symbol>\n");

	if(lex() != '{')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '{' </symbol>\n");

	statements();

	if(lex() != '}')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> '}' </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</whileStatement>\n");
	return 1;
}

int doStatement(void) {
	if(lex() != DO) {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<doStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> do </keyword>\n");

	subroutineCall();

	if(lex() != ';')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ';' </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</doStatement>\n");
	return 1;
}

int returnStatement(void) {
	if(lex() != RETURN) {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
		return 0;
	}
	PARSER_PRINT(parser.depth, "<returnStatement>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<keyword> return </keyword>\n");

	if(lex() == ';') {
		PARSER_PRINT(parser.depth, "<symbol> ';' </symbol>\n");
		--parser.depth;
		PARSER_PRINT(parser.depth, "</returnStatement>\n");
		return 1;
	} else {
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
		lexer.ptr = lexer.unget;
	}

	expression();

	if(lex() != ';')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ';' </symbol>\n");

	--parser.depth;
	PARSER_PRINT(parser.depth, "</returnStatement>\n");
	return 1;
}

/* expression: term (op term)* */
void expression(void) {
	if(term() == 0)
		return;
	PARSER_PRINT(parser.depth, "<expression>\n");
	++parser.depth;

	while(1) {
		lex();

		if(lexer.token != '+' && lexer.token != '-' &&
				lexer.token != '*' && lexer.token != '/' &&
				lexer.token != '&' && lexer.token != '|' &&
				lexer.token != '<' && lexer.token != '>' && lexer.token != '=') {
			if(lexer.text) {
				free(lexer.text);
				lexer.text = 0;
			}
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
	register char *tmp1, *tmp2;

	if(lex() == END)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);

	switch(lexer.token) {
	case NUMBER:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<integerConstant> %s </integerConstant>\n", lexer.text);
		free(lexer.text);
		break;
	case STRING:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<stringConstant> %s </stringConstant>\n", lexer.text);
		free(lexer.text);
		break;
	case TRUE: case FALSE: case NUL: case THIS:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<keywordConstant> %s </keywordConstant>\n",keyword[lexer.token-CLASS]);
		break;
	case ID:
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		tmp1 = lexer.unget;
		tmp2 = lexer.text;
		lex();

		if(lexer.token == '(' || lexer.token == '.') {
			lexer.ptr = tmp1;
			lexer.text = 0;
			free(tmp2);
			subroutineCall();
			break;
		}

		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", tmp2);
		free(tmp2);

		if(lexer.token != '[') {
			lexer.ptr = lexer.unget;
		} else {
			PARSER_PRINT(parser.depth, "<symbol> '[' </symbol>\n");
			expression();
			if(lex() != ']')
				error(1, 0, "parser error in %s line %d", __func__, __LINE__);
			PARSER_PRINT(parser.depth, "<symbol> ']' </symbol>\n");
		}
		break;
	case '(':
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<symbol> ( </symbol>\n");
		expression();
		if(lex() != ')')
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<symbol> ) </symbol>\n");
		break;
	case '-': case '~':
		PARSER_PRINT(parser.depth, "<term>\n");
		++parser.depth;
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);
		if(term() != 1)
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
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
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<subroutineCall>\n");
	++parser.depth;
	PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
	free(lexer.text);

	if(lex() != '.') {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
	} else {
		PARSER_PRINT(parser.depth, "<symbol> . </symbol>\n");
		if(lex() != ID)
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
		free(lexer.text);
	}

	if(lex() != '(')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> ( </symbol>\n");
	expressionList();
	if(lex() != ')')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
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
		PARSER_PRINT(parser.depth, "<symbol> ',' </symbol>\n");

		expression();
	}

	--parser.depth;
	PARSER_PRINT(parser.depth, "</expressionList>\n");
}

void parameterList(void) {
	/* type */
	if(lex() == ID) {
		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
		free(lexer.text);
	} else if(lexer.token >= INT && lexer.token <= VOID) {
		PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
	} else if(lexer.token == ')') { /* empty parameter list */
		lexer.ptr = lexer.unget;
		return;
	} else
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);

	/* varName */
	if(lex() != ID)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
	free(lexer.text);

	while(lex() == ',') {
		PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token);

		/* type */
		if(lex() == ID) {
			PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
			free(lexer.text);
		} else if(lexer.token >= INT && lexer.token <= VOID) {
			PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
		} else
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);

		/* varName */
		if(lex() != ID)
			error(1, 0, "parser error in %s line %d", __func__, __LINE__);
		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
		free(lexer.text);
	}
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
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
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
		PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text);
		free(lexer.text);
	} else if(lexer.token >= INT && lexer.token <= VOID) {
		PARSER_PRINT(parser.depth, "<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
	} else
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);

	/* subroutineName */
	if(lex() != ID)
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<identifier> %s </identifier>\n", lexer.text); /* subroutineName */
	free(lexer.text);

	/* parameterList */
	if(lex() != '(')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
	PARSER_PRINT(parser.depth, "<symbol> %c </symbol>\n", lexer.token); /* open paren */
	PARSER_PRINT(parser.depth, "<parameterList>\n");
	++parser.depth;
	parameterList();
	if(lex() != ')')
		error(1, 0, "parser error in %s line %d", __func__, __LINE__);
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
		if(lexer.text) {
			free(lexer.text);
			lexer.text = 0;
		}
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

void debug_lex(char *text) {
	int t;

	lex();

	printf("<tokens>\n");
	while((t = lex()) != END && t != ILLEGAL) {
		switch(t) {
		case KEYWORD:
			printf("<keyword> %s </keyword>\n", keyword[lexer.token - CLASS]);
			break;
		case SYMBOL:
			if(lexer.token == '<')
				printf("<symbol> &lt; </symbol>\n");
			if(lexer.token == '>')
				printf("<symbol> &gt; </symbol>\n");
			printf("<symbol> %c </symbol>\n", lexer.token);
			break;
		case NUMBER:
			printf("<integerConstant> %s </integerConstant>\n", lexer.text);
			free(lexer.text);
			break;
		case STRING:
			printf("<stringConstant> %s </stringConstant>\n", lexer.text);
			free(lexer.text);
			break;
		case ID:
			printf("<identifier> %s </identifier>\n", lexer.text);
			free(lexer.text);
			break;
		}
	}
	printf("</tokens>\n");
}

void debug_parse(AST_node *ast) {
}

int main(void) {
	atexit(cleanup);
	fmapopen("../11/Pong/PongGame.jack", O_RDONLY, &fm);
	fmapread(&fm);
	lexer.src = fm.buf;
	lexer.ptr = 0;
	parser.depth = 1;
	
	parse();

	fmapclose(&fm);
	return 0;
}
