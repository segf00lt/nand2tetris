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
#include "stb_ds.h"

#define ARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(x) (sizeof(x) / sizeof(*x)) - 1 /* compile-time strlen */

#define PARSER_PRINT(indent, fmt, ...) { \
	register unsigned int i = indent; \
	while(i > 0) { \
		putc(' ', xmlout); \
		putc(' ', xmlout); \
		--i; \
	} \
	fprintf(xmlout, fmt, ##__VA_ARGS__); \
}

#define STRPOOL_INIT(pool) pool.base = pool.free = malloc((pool.cap = 512))

#define AST_INIT(ast) ast.base = ast.free = malloc((ast.cap = 128) * sizeof(AST_node))

#define SYM_TAB_INIT(tab) {\
	tab.cap = 128;\
	tab.data = calloc(tab.cap, sizeof(Sym));\
	tab.count = 0;\
}

enum TOKENS {
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

enum SEGMENTS {
	ARGUMENT,
	LOCAL,
	THIS,
	THAT,
	TEMP,
	POINTER,
	STATIC,
	CONSTANT,
};

enum NODES {
	N_CLASS,
	N_CLASSNAME,
	N_CLASSVARDEC,
	N_SUBROUTINEDEC,
	N_PARAMETERLIST,
	N_SUBROUTINEBODY,
	N_VARDEC,
	N_STATEMENTS,
	N_LETSTATEMENT,
	N_IFSTATEMENT,
	N_ELSESTATEMENT,
	N_WHILESTATEMENT,
	N_DOSTATEMENT,
	N_RETURNSTATEMENT,
	N_EXPRESSIONLIST,
	N_EXPRESSION,
	N_TERM,
	N_SUBROUTINECALL,
	N_BINOP,
	N_UNOP,
	N_INTCONST,
	N_STRINGCONST,
	N_KEYCONST,
	N_STORAGEQUALIFIER,
	N_TYPE,
	N_VARNAME,
	N_SUBROUTINENAME,
};

char *nodes[] = {
	"CLASS",
	"CLASSVARDEC",
	"SUBROUTINEDEC",
	"PARAMETER",
	"SUBROUTINEBODY",
	"VARDEC",
	"STATEMENTS",
	"LETSTATEMENT",
	"IFSTATEMENT",
	"ELSESTATEMENT",
	"WHILESTATEMENT",
	"DOSTATEMENT",
	"RETURNSTATEMENT",
	"EXPRESSIONLIST",
	"EXPRESSION",
	"TERM",
	"SUBROUTINECALL",
	"BINOP",
	"UNOP",
	"INTCONST",
	"STRINGCONST",
	"KEYCONST",
	"STORAGEQUALIFIER",
	"TYPE",
	"VARNAME",
	"SUBROUTINENAME",
};

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

enum OPERATORS {
	OP_ADD = 0,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_AND,
	OP_OR,
	OP_LT,
	OP_GT,
	OP_EQ
};

char *operators[] = { "+", "-", "*", "/", "&", "|", "<", ">", "=" };

typedef struct {
	char *base, *free;
	size_t cap;
} Strpool;

typedef struct {
	char *src;
	char *ptr;
	char *text_s;
	char *text_e;
	char *unget;
	int token;
} Lexer;

typedef struct syntax_node {
	int id;
	char *val;
	struct syntax_node *left;
	struct syntax_node *right;
} AST_node;

typedef struct {
	AST_node *base;
	AST_node *free;
	size_t cap;
} AST;

typedef struct {
	char *name;
	char *kind; /* function type string */
	char *type; /* data type string */
	int seg; /* memory segment */
	int pos; /* position in memory segment */
} Sym;

typedef struct {
	Sym *data;
	size_t cap;
	size_t count;
} Sym_tab;

/* global variables */
Lexer lexer;
AST ast;
unsigned int depth;
Fmap fm; /* input file map */
FILE *xmlout;
Sym_tab classtab;
Sym_tab functab;
char classname[64];
char funcname[64];
Strpool spool;

/* utility functions */
char* str_alloc(Strpool *pool, size_t len, char *s);
void cleanup(void);

/* symbol table functions */
void sym_tab_grow(Sym_tab *tab);
void sym_tab_def(Sym_tab *tab, Sym *symbol);
size_t sym_tab_hash(Sym_tab *tab, char *name);
Sym* sym_tab_look(Sym_tab *tab, char *name);
void sym_tab_clear(Sym_tab *tab);
void sym_tab_print(Sym_tab *tab);

/* lexer functions */
int lex(void);

/* AST functions */
AST_node* ast_alloc_node(AST *ast, int id, char *val);

/* parser functions */
void parse(void);
AST_node* class(void);
AST_node* classVarDec(void);
AST_node* subroutineDec(void);
AST_node* parameterList(void);
AST_node* subroutineBody(void);
AST_node* varDec(void);
AST_node* statements(void);
AST_node* letStatement(void);
AST_node* ifStatement(void);
AST_node* whileStatement(void);
AST_node* doStatement(void);
AST_node* returnStatement(void);
AST_node* expressionList(void);
AST_node* expression(void);
AST_node* term(void);
AST_node* subroutineCall(void);

char* str_alloc(Strpool *pool, size_t len, char *s) {
	size_t count, i;
	char *p;

	count = pool->free - pool->base;
	if(count >= pool->cap) {
		pool->base = realloc(pool->base, (pool->cap <<= 1));
		pool->free = pool->base + count;
	}

	for(i = 0; i < len; ++i) pool->free[i] = s[i];
	pool->free[i] = 0;

	p = pool->free;
	pool->free += i + 1;

	return p;
}

void cleanup(void) {
	free(spool.base);
	free(classtab.data);
	free(ast.base);
	//free(functab.data);
	fmapclose(&fm);
	fclose(xmlout);
}

void sym_tab_grow(Sym_tab *tab) {
	Sym *old_data = tab->data;
	size_t old_cap = tab->cap;
	tab->data = calloc((tab->cap <<= 1), sizeof(Sym));
	for(size_t i = 0; i < old_cap; ++i) {
		if(!old_data[i].name) continue;
		sym_tab_def(tab, old_data + i);
	}
	free(old_data);
}

size_t sym_tab_hash(Sym_tab *tab, char *name) {
	size_t hash = 0;
	while(*name) hash += *(name++) * 31;
	return hash % tab->cap;
}

void sym_tab_def(Sym_tab *tab, Sym *symbol) {
	if(tab->count >= tab->cap) sym_tab_grow(tab);
	size_t i = sym_tab_hash(tab, symbol->name);
	while(tab->data[i].name) {
		if(!strcmp(tab->data[i].name, symbol->name)) return;
		i = (i + 1) % tab->cap;
	}
	tab->data[i] = *symbol;
	++tab->count;
}

void sym_tab_clear(Sym_tab *tab) {
	/* NOTE make sure you still have a pointer to the string pool when you call this */
	tab->count = 0;
	for(size_t i = 0; i < tab->cap; ++i) tab->data[i].name = 0;
}

void sym_tab_print(Sym_tab *tab) {
	for(size_t i = 0; i < tab->cap; ++i) {
		if(tab->data[i].name == 0) continue;
		fprintf(stderr, "SYMBOL\n\tname: %s\n\tkind: %s\n\ttype: %s\n\tseg: %s\n\tpos: %i\n",
				tab->data[i].name, tab->data[i].kind, tab->data[i].type,
				segments[tab->data[i].seg - ARGUMENT], tab->data[i].pos);
	}
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
				if(r[2] == '*') r += 1;
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

AST_node* ast_alloc_node(AST *ast, int id, char *val) {
	AST_node *newbase;
	size_t i;
	if(ast->free - ast->base >= ast->cap) {
		i = ast->cap - 1;
		ast->cap <<= 1;
		newbase = realloc(ast->base, sizeof(AST_node) * ast->cap);
		for(; i; --i) {
			newbase[i].left = (ast->base[i].left - ast->base) + newbase;
			newbase[i].right = (ast->base[i].right - ast->base) + newbase;
		}
	}

	*(ast->free) = (AST_node){ .id = id, .val = val };
	return ast->free++;
}

AST_node* class(void) {
	AST_node *root, *child;

	if(lex() != T_CLASS) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	fprintf(xmlout,"<class>\n");
	PARSER_PRINT(depth, "<keyword> class </keyword>\n");

	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	strncpy(classname, lexer.text_s, lexer.text_e - lexer.text_s);
	root = ast_alloc_node(&ast, N_CLASS, classname);

	if(lex() != '{') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	root->left = child = classVarDec();
	while((child->right = classVarDec()))
		child = child->right;

	root->right = child = subroutineDec();
	while((child->right = subroutineDec()))
		child = child->right;

	if(lex() != '}') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
	fprintf(xmlout,"</class>\n");

	return root;
}

AST_node* classVarDec(void) {
	AST_node *root, *child;

	lex();
	if(lexer.token != T_STATIC && lexer.token != T_FIELD) {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
		return 0;
	}

	PARSER_PRINT(depth, "<classVarDec>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

	root = ast_alloc_node(&ast, N_CLASSVARDEC, 0);
	/* first node in classVarDec says if is static of field */
	root->left = child = ast_alloc_node(&ast, N_STORAGEQUALIFIER, keyword[lexer.token - T_CLASS]);

	lex();
	if(lexer.token ==T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

		child->right = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);

	} else if(lexer.token == T_ID) { /* class type */
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->right = ast_alloc_node(&ast, N_TYPE, 0);
		child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	child = child->right;

	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	child->right = ast_alloc_node(&ast, N_VARNAME, 0);
	child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->right;

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->right = ast_alloc_node(&ast, N_VARNAME, 0);
		child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child = child->right;
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	--depth;
	PARSER_PRINT(depth, "</classVarDec>\n");

	return root;
}

AST_node* subroutineDec(void) {
	AST_node *root, *child;

	lex();
	if(lexer.token != T_CONSTRUCT && lexer.token != T_FUNC && lexer.token != T_METHOD) {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
		return 0;
	}

	PARSER_PRINT(depth, "<subroutineDec>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

	root = ast_alloc_node(&ast, N_SUBROUTINEDEC, 0);
	/* storage qualifiers say whether a subroutine is a constructor a function or a method */
	root->left = child = ast_alloc_node(&ast, N_STORAGEQUALIFIER, keyword[lexer.token - T_CLASS]);

	/* return type */
	lex();
	if(lexer.token == T_ID) {
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->right = ast_alloc_node(&ast, N_TYPE, 0);
		child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

		child->right = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	child = child->right;

	/* subroutineName */
	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	child->right = ast_alloc_node(&ast, N_SUBROUTINENAME, 0);
	child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->right;

	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	/* parameterList */
	if(lex() != '(') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
	PARSER_PRINT(depth, "<parameterList>\n");
	++depth;

	child->right = parameterList();
	if(child->right) child = child->right; /* empty parameterList returns NULL */

	if(lex() != ')') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	--depth;
	PARSER_PRINT(depth, "</parameterList>\n");
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	child->right = subroutineBody();
	child = child->right;

	--depth;
	PARSER_PRINT(depth, "</subroutineDec>\n");

	return root;
}

AST_node* parameterList(void) {
	/* parameterList is a sequence of type followed by varName */
	AST_node *root, *child;

	/* type */
	if(lex() == T_ID) {
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child = ast_alloc_node(&ast, N_TYPE, 0);
		child->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

		child = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);

	} else if(lexer.token == ')') { /* empty parameter list */
		lexer.ptr = lexer.unget;
		return 0;
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	root = ast_alloc_node(&ast, N_PARAMETERLIST, 0);
	root->left = child;

	/* varName */
	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	child->right = ast_alloc_node(&ast, N_VARNAME, 0);
	child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->right;

	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		/* type */
		if(lex() == T_ID) {
			PARSER_PRINT(depth, "<identifier> ");
			fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
			fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

			child->right = ast_alloc_node(&ast, N_TYPE, 0);
			child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

		} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {
			PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

			child->right = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);
		} else
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		child = child->right;

		/* varName */
		if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->right = ast_alloc_node(&ast, N_VARNAME, 0);
		child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child = child->right;
	}

	lexer.ptr = lexer.unget;

	return root;
}

AST_node* subroutineBody(void) {
	AST_node *root, *child;

	if(lex() != '{') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<subroutineBody>\n");
	++depth;
	PARSER_PRINT(depth, "<symbol> { </symbol>\n");

	root = ast_alloc_node(&ast, N_SUBROUTINEBODY, 0);

	root->left = child = varDec();
	while((child->right = varDec()))
		child = child->right;

	/* NOTE if this were a real language declarations would be statements
	 * so theres no point putting statements on the left side of this subtree */
	child->right = statements();

	if(lex() != '}') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> } </symbol>\n");
	--depth;
	PARSER_PRINT(depth, "</subroutineBody>\n");

	return root;
}

AST_node* varDec(void) {
	AST_node *root, *child;

	if(lex() != T_VAR) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_VARDEC, 0);

	PARSER_PRINT(depth, "<varDec>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> var </keyword>\n");

	lex();
	if(lexer.token == T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */
		PARSER_PRINT(depth, "<keyword> %s </keyword>\n", keyword[lexer.token - T_CLASS]);

		child = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);

	} else if(lexer.token == T_ID) { /* class type */
		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child = ast_alloc_node(&ast, N_TYPE, 0);
		child->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	root->left = child;

	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	child->right = ast_alloc_node(&ast, N_VARNAME, 0);
	child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->right;

	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	while(lex() == ',') {
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

		child->right = ast_alloc_node(&ast, N_VARNAME, 0);
		child->right->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child = child->right;

		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

	--depth;
	PARSER_PRINT(depth, "</varDec>\n");

	return root;
}

AST_node* statements(void) {
	AST_node *root, *child;

	PARSER_PRINT(depth, "<statements>\n");
	++depth;

	root = ast_alloc_node(&ast, N_STATEMENTS, 0);

	if((child = letStatement())) root->left = child;
	if((child = ifStatement())) root->left = child;
	if((child = whileStatement())) root->left = child;
	if((child = doStatement())) root->left = child;
	if((child = returnStatement())) root->left = child;

	for(;; child = child->right) {
		if((child->right = letStatement())) continue;
		if((child->right = ifStatement())) continue;
		if((child->right = whileStatement())) continue;
		if((child->right = doStatement())) continue;
		if((child->right = returnStatement())) continue;

		break;
	}

	--depth;
	PARSER_PRINT(depth, "</statements>\n");

	return root;
}

AST_node* letStatement(void) {
	AST_node *root, *child;

	if(lex() != T_LET) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_LETSTATEMENT, 0);

	PARSER_PRINT(depth, "<letStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> let </keyword>\n");

	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	root->left = child = ast_alloc_node(&ast, N_VARNAME, 0);
	child->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	if(lex() != '[') {
		lexer.ptr = lexer.unget;
	} else {
		PARSER_PRINT(depth, "<symbol> [ </symbol>\n");

		child->right = expression();
		child = child->right;

		if(lex() != ']') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> ] </symbol>\n");
	}

	if(lex() != '=') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	child->right = ast_alloc_node(&ast, N_BINOP, operators[OP_EQ]);
	child = child->right;
	PARSER_PRINT(depth, "<symbol> = </symbol>\n");

	child->right = expression();

	if(lex() != ';') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ; </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</letStatement>\n");
	return root;
}

AST_node* ifStatement(void) {
	AST_node *root, *child;

	if(lex() != T_IF) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_IFSTATEMENT, 0);

	PARSER_PRINT(depth, "<ifStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> if </keyword>\n");

	if(lex() != '(') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ( </symbol>\n");

	root->left = child = expression();

	if(lex() != ')') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ) </symbol>\n");

	if(lex() != '{') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> { </symbol>\n");

	child->right = statements();
	if(child->right) child = child->right; /* TODO can statements return 0 ???? */

	if(lex() != '}') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> } </symbol>\n");

	if(lex() == T_ELSE) {
		PARSER_PRINT(depth, "<keyword> else </keyword>\n");

		if(lex() != '{') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> { </symbol>\n");

		child->right = statements();

		if(lex() != '}') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> } </symbol>\n");
	} else
		lexer.ptr = lexer.unget;

	--depth;
	PARSER_PRINT(depth, "</ifStatement>\n");
	return root;
}

AST_node* whileStatement(void) {
	AST_node *root, *child;

	if(lex() != T_WHILE) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_WHILESTATEMENT, 0);

	PARSER_PRINT(depth, "<whileStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> while </keyword>\n");

	if(lex() != '(') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ( </symbol>\n");

	root->left = child = expression();

	if(lex() != ')') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ) </symbol>\n");

	if(lex() != '{') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> { </symbol>\n");

	child->right = statements();

	if(lex() != '}') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> } </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</whileStatement>\n");
	return root;
}

AST_node* doStatement(void) {
	AST_node *root;

	if(lex() !=T_DO) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_DOSTATEMENT, 0);
	PARSER_PRINT(depth, "<doStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> do </keyword>\n");

	root->left = subroutineCall();

	if(lex() != ';') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ; </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</doStatement>\n");
	return root;
}

AST_node* returnStatement(void) {
	AST_node *root;

	if(lex() != T_RETURN) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_RETURNSTATEMENT, 0);

	PARSER_PRINT(depth, "<returnStatement>\n");
	++depth;
	PARSER_PRINT(depth, "<keyword> return </keyword>\n");

	if(lex() == ';') {
		PARSER_PRINT(depth, "<symbol> ; </symbol>\n");
		--depth;
		PARSER_PRINT(depth, "</returnStatement>\n");
		return root;
	} else
		lexer.ptr = lexer.unget;

	root->left = expression();

	if(lex() != ';') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ; </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</returnStatement>\n");
	return root;
}

AST_node* expression(void) {
	AST_node *root, *child;

	PARSER_PRINT(depth, "<expression>\n");
	++depth;
	if(term() == 0) {
		--depth;
		PARSER_PRINT(depth, "</expression>\n");
		return 0;
	}

	root = ast_alloc_node(&ast, N_EXPRESSION, 0);

	while(1) {
		lex();

		/* TODO replace operator characters with op OP_xxx */
		if(lexer.token != '+' && lexer.token != '-' &&
				lexer.token != '*' && lexer.token != '/' &&
				lexer.token != '&' && lexer.token != '|' &&
				lexer.token != '<' && lexer.token != '>' && lexer.token != '=') {
			lexer.ptr = lexer.unget;
			lexer.token = 0;
			break;
		}
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);

		child->right = term();
		if(child->right) child = child->right; /* TODO can term return 0 ???? */
	}

	--depth;
	PARSER_PRINT(depth, "</expression>\n");

	return root;
}

/* TODO term() */
AST_node* term(void) {
	AST_node *root, *child;
	register char *tmp1, *tmp2, *tmp3;

	if(lex() == T_END)
		error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	switch(lexer.token) {
	case T_NUMBER:
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<integerConstant> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </integerConstant>\n", 1, STRLEN(" </integerConstant>\n"), xmlout);
		break;
	case T_STRING:
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<stringConstant> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </stringConstant>\n", 1, STRLEN(" </stringConstant>\n"), xmlout);
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
		fwrite(tmp2, 1, tmp3 - tmp2, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		if(lexer.token != '[') {
			lexer.ptr = lexer.unget;
		} else {
			PARSER_PRINT(depth, "<symbol> [ </symbol>\n");
			expression();
			if(lex() != ']') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
			PARSER_PRINT(depth, "<symbol> ] </symbol>\n");
		}
		break;
	case '(':
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<symbol> ( </symbol>\n");
		expression();
		if(lex() != ')') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		PARSER_PRINT(depth, "<symbol> ) </symbol>\n");
		break;
	case '-': case '~':
		PARSER_PRINT(depth, "<term>\n");
		++depth;
		PARSER_PRINT(depth, "<symbol> %c </symbol>\n", lexer.token);
		if(!term())
			error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
		break;
	default:
		lexer.token = 0;
		lexer.ptr = lexer.unget;
		return 0;
	}

	--depth;
	PARSER_PRINT(depth, "</term>\n");
	return root;
}

AST_node* subroutineCall(void) {
	AST_node *root, *child;
	char *tmp0, *tmp1;

	if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

	root = ast_alloc_node(&ast, N_SUBROUTINECALL, 0);

	PARSER_PRINT(depth, "<subroutineCall>\n");
	++depth;
	PARSER_PRINT(depth, "<identifier> ");
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);
	tmp0 = lexer.text_s;
	tmp1 = lexer.text_e;

	if(lex() != '.') {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
		root->left = ast_alloc_node(&ast, N_SUBROUTINENAME, 0);
		root->left->val = str_alloc(&spool, tmp1 - tmp0, tmp0);
	} else {
		root->left = ast_alloc_node(&ast, N_CLASSNAME, 0);
		root->left->val = str_alloc(&spool, tmp1 - tmp0, tmp0);

		PARSER_PRINT(depth, "<symbol> . </symbol>\n");
		if(lex() != T_ID) error(1, 0, "parser error in %s source line %d", __func__, __LINE__);

		child = ast_alloc_node(&ast, N_SUBROUTINENAME, 0);
		child->val = str_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		root->left->right = child;

		PARSER_PRINT(depth, "<identifier> ");
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);
	}

	if(lex() != '(') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ( </symbol>\n");
	child->right = expressionList();
	if(lex() != ')') error(1, 0, "parser error in %s source line %d", __func__, __LINE__);
	PARSER_PRINT(depth, "<symbol> ) </symbol>\n");

	--depth;
	PARSER_PRINT(depth, "</subroutineCall>\n");
	return root;
}

AST_node* expressionList(void) {
	AST_node *root, *child;
	PARSER_PRINT(depth, "<expressionList>\n");
	++depth;

	root = ast_alloc_node(&ast, N_EXPRESSIONLIST, 0);

	child = expression();
	root->left = child;
	while(1) {
		if(lex() != ',') {
			lexer.token = 0;
			lexer.ptr = lexer.unget;
			break;
		}
		PARSER_PRINT(depth, "<symbol> , </symbol>\n");

		child->right = expression();
		child = child->right;
	}

	--depth;
	PARSER_PRINT(depth, "</expressionList>\n");
	return root;
}

void parse(void) {
	lex();
	class();
}

int main(int argc, char *argv[]) {
	atexit(cleanup);

	fmapopen(argv[1], O_RDONLY, &fm);
	fmapread(&fm);
	xmlout = fopen("xmlout", "w");

	lexer.src = fm.buf;
	lexer.ptr = 0;

	depth = 1;
	STRPOOL_INIT(spool);
	SYM_TAB_INIT(classtab);
	AST_INIT(ast);

	parse();
	
	return 0;
}
