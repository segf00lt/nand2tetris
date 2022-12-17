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
#include <assert.h>
#include "fmap.c"

#define ARRLEN(x) (sizeof(x) / sizeof(*x))
#define STRLEN(x) (sizeof(x) / sizeof(*x)) - 1 /* compile-time strlen */

#define AST_PAGE_SIZE 256
#define STRPOOL_PAGE_SIZE 512
#define AST_INITIAL_PAGE_COUNT 4
#define STRPOOL_INITIAL_PAGE_COUNT 4

#define PARSER_PRINT(indent, fmt, ...) { \
	register unsigned int i = indent; \
	while(i > 0) { \
		putc(' ', xmlout); \
		putc(' ', xmlout); \
		--i; \
	} \
	fprintf(xmlout, fmt, ##__VA_ARGS__); \
}

#define STRPOOL_INIT(pool) { \
	pool.pages = malloc(STRPOOL_INITIAL_PAGE_COUNT * sizeof(char*)); \
	pool.cap = STRPOOL_INITIAL_PAGE_COUNT; \
	pool.cur = 0; \
	pool.pages[pool.cur] = malloc(STRPOOL_PAGE_SIZE); \
	pool.base = pool.free = pool.pages[ast.cur]; \
}

#define AST_INIT(ast) { \
	ast.pages = malloc(AST_INITIAL_PAGE_COUNT * sizeof(AST_node*)); \
	ast.cap = AST_INITIAL_PAGE_COUNT; \
	ast.cur = 0; \
	ast.nodecount = 1; \
	ast.pages[ast.cur] = malloc(AST_PAGE_SIZE * sizeof(AST_node)); \
	ast.base = ast.free = ast.pages[ast.cur]; \
}

#define SYM_TAB_INIT(tab) {\
	tab.cap = 128;\
	tab.data = calloc(tab.cap, sizeof(Sym));\
	tab.count = tab.static_count = tab.this_count = tab.arg_count = tab.local_count = 0;\
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
	T_TRUE, T_FALSE, T_NULL, T_THIS,
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
	S_ARGUMENT,
	S_LOCAL,
	S_THIS,
	S_THAT,
	S_TEMP,
	S_POINTER,
	S_STATIC,
	S_CONSTANT,
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

enum NODES {
	N_CLASS = 1,
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
	N_OP,
	N_UNOP,
	N_INTCONST,
	N_STRINGCONST,
	N_KEYCONST,
	N_STORAGEQUALIFIER,
	N_TYPE,
	N_CLASSNAME,
	N_VARNAME,
	N_SUBROUTINENAME,
};

char *nodes[] = {
	0,
	"CLASS",
	"CLASSVARDEC",
	"SUBROUTINEDEC",
	"PARAMETERLIST",
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
	"OP",
	"UNOP",
	"INTCONST",
	"STRINGCONST",
	"KEYCONST",
	"STORAGEQUALIFIER",
	"TYPE",
	"CLASSNAME",
	"VARNAME",
	"SUBROUTINENAME",
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
	OP_NOT,
	OP_AND,
	OP_OR,
	OP_LT,
	OP_GT,
	OP_EQ,
	OP_INDEX,
	OP_COMMA,
};

char *operators[] = { "+", "-", "*", "/", "~", "&", "|", "<", ">", "=", "[", "," };

typedef struct {
	char *base;
	char *free;
	char **pages;
	size_t cur;
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
	unsigned int id; /* debug */
	int kind;
	char *val;
	struct syntax_node *down; /* down */
	struct syntax_node *next; /* across */
} AST_node;

typedef struct {
	/* since we never free individual nodes *free stores the first vacant
	 * node in the current page */
	AST_node *free;
	AST_node *base; /* base of current page */
	AST_node **pages;
	size_t cur; /* index of current page */
	size_t cap; /* capacity of **pages */
	size_t nodecount; /* debug */
} AST;

typedef struct {
	int pos; /* position in memory segment */
	int seg; /* memory segment */
	char *type; /* data type string */
	char *name;
} Sym;

typedef struct {
	Sym *data;
	char *name;
	size_t cap;
	size_t count;
	size_t static_count;
	size_t this_count;
	size_t arg_count;
	size_t local_count;
} Sym_tab;

/* global variables */
Lexer lexer;
AST ast;
unsigned int depth;
Fmap fm; /* input file map */
FILE *xmlout;
FILE *astout; /* debug output for ast */
Sym_tab classtab;
Sym_tab functab;
char classname[64];
Strpool spool;

/* utility functions */
char* strpool_alloc(Strpool *pool, size_t len, char *s);
void strpool_free(Strpool *pool);
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
AST_node* ast_alloc_node(AST *ast, int kind, char *val);
void ast_free(AST *ast);
void debug_ast_alloc(void);

/* parser functions */
void debug_parser(AST_node *node);
AST_node* parse(void);
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

char* strpool_alloc(Strpool *pool, size_t len, char *s) {
	size_t count, i;
	char *p;

	count = pool->free - pool->base;
	if(count + len >= STRPOOL_PAGE_SIZE) {
		++pool->cur;
		if(pool->cur >= pool->cap) {
			pool->cap <<= 1;
			pool->pages = realloc(pool->pages, pool->cap * sizeof(char*));
		}
		pool->pages[pool->cur] = malloc(STRPOOL_PAGE_SIZE);
		pool->base = pool->free = pool->pages[pool->cur];
	}

	for(i = 0; i < len; ++i) pool->free[i] = s[i];
	pool->free[i] = 0;

	p = pool->free;
	pool->free += i + 1;

	return p;
}

void strpool_free(Strpool *pool) {
	for(size_t i = 0; i <= pool->cur; ++i)
		free(pool->pages[i]);
	free(pool->pages);
}

AST_node* ast_alloc_node(AST *ast, int kind, char *val) {
	if(ast->free - ast->base >= AST_PAGE_SIZE) {
		++ast->cur;
		if(ast->cur >= ast->cap) {
			ast->cap <<= 1;
			ast->pages = realloc(ast->pages, ast->cap * sizeof(AST_node*));
		}
		ast->pages[ast->cur] = malloc(AST_PAGE_SIZE * sizeof(AST_node));
		ast->base = ast->free = ast->pages[ast->cur];
	}

	*(ast->free) = (AST_node){ .id = ast->nodecount++, .kind = kind, .val = val, .down = 0, .next = 0 };
	return ast->free++;
}

void ast_free(AST *ast) {
	for(size_t i = 0; i <= ast->cur; ++i)
		free(ast->pages[i]);
	free(ast->pages);
}

void cleanup(void) {
	strpool_free(&spool);
	ast_free(&ast);
	free(classtab.data);
	free(functab.data);
	fmapclose(&fm);
	fclose(xmlout);
	fclose(astout);
}

void sym_tab_grow(Sym_tab *tab) {
	Sym *old_data = tab->data;
	size_t old_cap = tab->cap;
	tab->data = calloc((tab->cap <<= 1), sizeof(Sym));
	for(size_t i = 0; i < old_cap; ++i) {
		if(!old_data[i].name)
			continue;
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
		if(!strcmp(tab->data[i].name, symbol->name))
			return;
		i = (i + 1) % tab->cap;
	}
	tab->data[i] = *symbol;
	++tab->count;
}

void sym_tab_clear(Sym_tab *tab) {
	/* NOTE make sure you still have a pointer to the string pool when you call this */
	tab->count = tab->static_count = tab->this_count = tab->arg_count = tab->local_count = 0;
	for(size_t i = 0; i < tab->cap; ++i) tab->data[i].name = 0;
}

void sym_tab_print(Sym_tab *tab) {
	for(size_t i = 0; i < tab->cap; ++i) {
		if(tab->data[i].name == 0) continue;
		fprintf(stderr, "SYMBOL\n\tname: %s\n\ttype: %s\n\tseg: %s\n\tpos: %i\n\n",
				tab->data[i].name, tab->data[i].type,
				segments[tab->data[i].seg], tab->data[i].pos);
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
		check = tp[keyword_length[i]];
		if(strstr(tp,keyword[i]) == tp &&
				(check == ' ' ||
				 check == ';' ||
				 check == '(' ||
				 check == '{')
		  )
		{
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

AST_node* class(void) {
	AST_node *root, *child;

	if(lex() != T_CLASS) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);


	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	strncpy(classname, lexer.text_s, lexer.text_e - lexer.text_s);
	root = ast_alloc_node(&ast, N_CLASS, classname);

	if(lex() != '{') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	root->down = child = classVarDec();
	if(!child) {
		root->down = child = subroutineDec();
	} else {
		while((child->next = classVarDec()))
			child = child->next;
		child->next = subroutineDec();
		if(child->next) child = child->next;
	}

	while((child->next = subroutineDec()))
		child = child->next;

	if(lex() != '}') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

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

	++depth;

	root = ast_alloc_node(&ast, N_CLASSVARDEC, 0);
	/* first node in classVarDec says if is static of field */
	root->down = child = ast_alloc_node(&ast, N_STORAGEQUALIFIER, keyword[lexer.token - T_CLASS]);

	lex();
	if(lexer.token ==T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */

		child->next = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);

	} else if(lexer.token == T_ID) { /* class type */
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->next = ast_alloc_node(&ast, N_TYPE, 0);
		child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	child = child->next;

	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	child->next = ast_alloc_node(&ast, N_VARNAME, 0);
	child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->next;

	while(lex() == ',') {

		if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->next = ast_alloc_node(&ast, N_VARNAME, 0);
		child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child = child->next;
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;

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

	++depth;

	root = ast_alloc_node(&ast, N_SUBROUTINEDEC, 0);
	/* storage qualifiers say whether a subroutine is a constructor a function or a method */
	root->down = child = ast_alloc_node(&ast, N_STORAGEQUALIFIER, keyword[lexer.token - T_CLASS]);

	/* return type */
	lex();
	if(lexer.token == T_ID) {
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->next = ast_alloc_node(&ast, N_TYPE, 0);
		child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {

		child->next = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);
	} else
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	child = child->next;

	/* subroutineName */
	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	child->next = ast_alloc_node(&ast, N_SUBROUTINENAME, 0);
	child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->next;

	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	/* parameterList */
	if(lex() != '(') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	++depth;

	child->next = parameterList();
	child = child->next;

	if(lex() != ')') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	--depth;

	child->next = subroutineBody();
	child = child->next;

	--depth;

	return root;
}

AST_node* parameterList(void) {
	/* parameterList is a sequence of type followed by varName */
	AST_node *root, *child;

	root = ast_alloc_node(&ast, N_PARAMETERLIST, 0);

	/* type */
	if(lex() == T_ID) {
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child = ast_alloc_node(&ast, N_TYPE, 0);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {

		child = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);

	} else if(lexer.token == ')') { /* empty parameter list */
		lexer.ptr = lexer.unget;
		return root;
	} else
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	root->down = child;

	/* varName */
	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	child->next = ast_alloc_node(&ast, N_VARNAME, 0);
	child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->next;

	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	while(lex() == ',') {

		/* type */
		if(lex() == T_ID) {
			fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
			fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

			child->next = ast_alloc_node(&ast, N_TYPE, 0);
			child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

		} else if(lexer.token >=T_INT && lexer.token <= T_VOID) {

			child->next = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);
		} else
			error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		child = child->next;

		/* varName */
		if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child->next = ast_alloc_node(&ast, N_VARNAME, 0);
		child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child = child->next;
	}

	lexer.ptr = lexer.unget;

	return root;
}

AST_node* subroutineBody(void) {
	AST_node *root, *child;

	if(lex() != '{') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	++depth;

	root = ast_alloc_node(&ast, N_SUBROUTINEBODY, 0);

	/* NOTE if this were a real language declarations would be statements
	 * so theres no point putting statements on the down side of this subtree */

	if((root->down = child = varDec())) {
		while((child->next = varDec()))
			child = child->next;

		child->next = statements();
	} else
		root->down = child = statements();

	if(lex() != '}') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	--depth;

	return root;
}

AST_node* varDec(void) {
	AST_node *root, *child;

	if(lex() != T_VAR) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_VARDEC, 0);

	++depth;

	lex();
	if(lexer.token == T_INT || lexer.token == T_CHAR || lexer.token == T_BOOL) { /* builtin type */

		child = ast_alloc_node(&ast, N_TYPE, keyword[lexer.token - T_CLASS]);

	} else if(lexer.token == T_ID) { /* class type */
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child = ast_alloc_node(&ast, N_TYPE, 0);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	} else
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	root->down = child;

	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	child->next = ast_alloc_node(&ast, N_VARNAME, 0);
	child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
	child = child->next;

	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	while(lex() == ',') {

		if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

		child->next = ast_alloc_node(&ast, N_VARNAME, 0);
		child->next->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		child = child->next;

		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);
	}

	if(lexer.token != ';')
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;

	return root;
}

AST_node* statements(void) {
	AST_node *root, *child;

	++depth;

	root = ast_alloc_node(&ast, N_STATEMENTS, 0);

	if((child = letStatement())) root->down = child;
	else if((child = ifStatement())) root->down = child;
	else if((child = whileStatement())) root->down = child;
	else if((child = doStatement())) root->down = child;
	else if((child = returnStatement())) root->down = child;

	for(;; child = child->next) {
		if((child->next = letStatement())) continue;
		if((child->next = ifStatement())) continue;
		if((child->next = whileStatement())) continue;
		if((child->next = doStatement())) continue;
		if((child->next = returnStatement())) continue;

		break;
	}

	--depth;

	return root;
}

AST_node* letStatement(void) {
	AST_node *root, *child;

	if(lex() != T_LET) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_LETSTATEMENT, 0);

	++depth;

	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	root->down = child = ast_alloc_node(&ast, N_VARNAME, 0);
	child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);

	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

	if(lex() != '[') {
		lexer.ptr = lexer.unget;
	} else {

		child->next = expression();
		child = child->next;

		if(lex() != ']') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	}

	if(lex() != '=') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	/* we can ommit the = in let statements */

	child->next = expression();

	if(lex() != ';') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;
	return root;
}

AST_node* ifStatement(void) {
	AST_node *root, *child;

	if(lex() != T_IF) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_IFSTATEMENT, 0);

	++depth;

	if(lex() != '(') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	root->down = child = expression();

	if(lex() != ')') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	if(lex() != '{') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	child->next = statements();
	if(child->next) child = child->next;

	if(lex() != '}') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	if(lex() == T_ELSE) {

		if(lex() != '{') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

		child->next = ast_alloc_node(&ast, N_ELSESTATEMENT, 0);
		child = child->next;
		child->next = statements();

		if(lex() != '}') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	} else
		lexer.ptr = lexer.unget;

	--depth;
	return root;
}

AST_node* whileStatement(void) {
	AST_node *root, *child;

	if(lex() != T_WHILE) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_WHILESTATEMENT, 0);

	++depth;

	if(lex() != '(') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	root->down = child = expression();

	if(lex() != ')') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	if(lex() != '{') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	child->next = statements();

	if(lex() != '}') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;
	return root;
}

AST_node* doStatement(void) {
	AST_node *root;

	if(lex() !=T_DO) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_DOSTATEMENT, 0);
	++depth;

	root->down = subroutineCall();

	if(lex() != ';') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;
	return root;
}

AST_node* returnStatement(void) {
	AST_node *root;

	if(lex() != T_RETURN) {
		lexer.ptr = lexer.unget;
		return 0;
	}

	root = ast_alloc_node(&ast, N_RETURNSTATEMENT, 0);

	++depth;

	if(lex() == ';') {
		--depth;
		return root;
	} else
		lexer.ptr = lexer.unget;

	root->down = expression();

	if(lex() != ';') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;
	return root;
}

AST_node* expression(void) {
	AST_node *root, *child;

	++depth;
	if(!(child = term())) {
		--depth;
		return 0;
	}

	root = ast_alloc_node(&ast, N_EXPRESSION, 0);
	root->down = child;

	while(1) {
		lex();

		register int i;
		for(i = 0; i < ARRLEN(operators) && operators[i][0] != lexer.token; ++i);

		if(i >= ARRLEN(operators) || operators[i][0] == ',') {
			lexer.ptr = lexer.unget;
			lexer.token = 0;
			break;
		}


		child->next = ast_alloc_node(&ast, N_OP, operators[i]);
		child = child->next;

		child->next = term();
		if(child->next)
			child = child->next;
		else
			error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	}

	--depth;

	return root;
}

AST_node* term(void) {
	AST_node *root, *child;
	register char *tmp1, *tmp2, *tmp3;

	if(lex() == T_END)
		error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	if(!(lexer.token >= T_NUMBER && lexer.token <= T_ID) &&
	   !(lexer.token >= T_TRUE && lexer.token <= T_THIS) && 
	   lexer.token != '(' && lexer.token != '-' && lexer.token != '~') {
		lexer.token = 0;
		lexer.ptr = lexer.unget;
		return 0;
	}

	++depth;

	root = ast_alloc_node(&ast, N_TERM, 0);

	switch(lexer.token) {
	case T_NUMBER:
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </integerConstant>\n", 1, STRLEN(" </integerConstant>\n"), xmlout);

		child = ast_alloc_node(&ast, N_INTCONST, 0);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_STRING:
		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </stringConstant>\n", 1, STRLEN(" </stringConstant>\n"), xmlout);

		child = ast_alloc_node(&ast, N_STRINGCONST, 0);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		break;
	case T_TRUE: case T_FALSE: case T_NULL: case T_THIS:
		child = ast_alloc_node(&ast, N_KEYCONST, keyword[lexer.token - T_CLASS]);
		break;
	case T_ID:
		tmp1 = lexer.unget;
		tmp2 = lexer.text_s;
		tmp3 = lexer.text_e;
		lex();

		if(lexer.token == '(' || lexer.token == '.') {
			lexer.ptr = tmp1;
			child = subroutineCall();
			break;
		}

		fwrite(tmp2, 1, tmp3 - tmp2, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);

		child = ast_alloc_node(&ast, N_VARNAME, 0);
		child->val = strpool_alloc(&spool, tmp3 - tmp2, tmp2);

		if(lexer.token != '[') {
			lexer.ptr = lexer.unget;
		} else {

			child->next = ast_alloc_node(&ast, N_OP, operators[OP_INDEX]);
			child->next->next = expression();

			if(lex() != ']') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		}
		break;
	case '(':

		child = expression();

		if(lex() != ')') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		break;
	case '-': case '~':
		child = ast_alloc_node(&ast, N_OP, operators[(lexer.token == '-') ? OP_SUB : OP_NOT]);
		if(!(child->next = term()))
			error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		break;
	}

	root->down = child;

	--depth;

	return root;
}

AST_node* subroutineCall(void) {
	AST_node *root, *child;
	char *tmp0, *tmp1;

	if(lex() != T_ID) error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	root = ast_alloc_node(&ast, N_SUBROUTINECALL, 0);

	++depth;
	fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
	fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);
	tmp0 = lexer.text_s;
	tmp1 = lexer.text_e;

	if(lex() != '.') {
		lexer.ptr = lexer.unget;
		lexer.token = 0;
		root->down = child = ast_alloc_node(&ast, N_SUBROUTINENAME, 0);
		child->val = strpool_alloc(&spool, tmp1 - tmp0, tmp0);
	} else {
		root->down = ast_alloc_node(&ast, N_CLASSNAME, 0);
		root->down->val = strpool_alloc(&spool, tmp1 - tmp0, tmp0);

		if(lex() != T_ID) {
			child = 0;
			error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
		}

		child = ast_alloc_node(&ast, N_SUBROUTINENAME, 0);
		child->val = strpool_alloc(&spool, lexer.text_e - lexer.text_s, lexer.text_s);
		root->down->next = child;

		fwrite(lexer.text_s, 1, lexer.text_e - lexer.text_s, xmlout);
		fwrite(" </identifier>\n", 1, STRLEN(" </identifier>\n"), xmlout);
	}

	if(lex() != '(') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);
	child->next = expressionList();
	if(lex() != ')') error(1, 0, "parser error in %s on compiler source line %d", __func__, __LINE__);

	--depth;
	return root;
}

AST_node* expressionList(void) {
	AST_node *root, *child;
	++depth;

	if(!(child = expression())) {
		--depth;
		return 0;
	}
	root = ast_alloc_node(&ast, N_EXPRESSIONLIST, 0);
	root->down = child;
	while(1) {
		if(lex() != ',') {
			lexer.token = 0;
			lexer.ptr = lexer.unget;
			break;
		}

		child->next = expression();
		child = child->next;
	}

	--depth;
	return root;
}

AST_node* parse(void) {
	lex();
	return class();
}

void debug_ast_alloc(void) {
	for(size_t i = 0; i <= ast.cur; ++i) {
		fprintf(stderr, "PAGE %zu\n", i);
		for(size_t j = 0; ast.pages[i][j].kind; ++j) {
			fprintf(stderr, "\taddress: %p\n"
					"\tid: %u\n"
					"\tkind: %s\n"
					"\tval: %s\n"
					"\tdown: %p\n"
					"\tnext: %p\n\n"
					, (void*)(ast.pages[i]+j),
					ast.pages[i][j].id,
					nodes[ast.pages[i][j].kind], ast.pages[i][j].val,
					(void*)(ast.pages[i][j].down), (void*)(ast.pages[i][j].next));
		}
	}
}

void debug_parser(AST_node *node, size_t depth) {
	for(; node; node = node->next) {
		int i;
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "id: %u\n", node->id);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "kind: %s\n", nodes[node->kind]);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "val: %s\n", node->val);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "down: %u\n", node->down ? node->down->id : 0);
		for(i = 0; i < depth; ++i) fwrite("*   ", 1, 4, astout);
		fprintf(astout, "next: %u\n\n", node->next ? node->next->id : 0);
		if(node->down) {
			++depth;
			debug_parser(node->down, depth);
			--depth;
		}
	}
}

/* TODO
 *
 * IMPORTANT refactor unit tests!!!!!!!!!!!!!!!!!!!!!!!
 *
 * parse_test()
 * compile()
 *
 */

void ast_to_xml(AST_node *node, size_t depth) {
	AST_node *child;
	if(node->kind == N_CLASS) {
		fprintf(xmlout, "<class>\n");
		fprintf(xmlout, "  <keyword> class </keyword>\n");
		fprintf(xmlout, "  <identifier> %s </identifier>\n", node->val);
		fprintf(xmlout, "  <symbol> { </symbol>\n");
		++depth;
		ast_to_xml(node->down, depth);
		--depth;
		fprintf(xmlout, "</class>\n");
		return;
	}
	for(; node; node = node->next) {
		switch(node->kind) {
		case N_SUBROUTINEDEC:
			PARSER_PRINT(depth, "<subroutineDec>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</subroutineDec>\n");
			break;
		case N_SUBROUTINEBODY:
			PARSER_PRINT(depth, "<subroutineBody>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</subroutineBody>\n");
			break;
		case N_PARAMETERLIST:
			PARSER_PRINT(depth, "<symbol> ( </symbol>\n");
			PARSER_PRINT(depth, "<parameterList>");
			++depth;
			for(child = node->down; child; child = child->next) {
				if(child->val >= keyword[T_INT-T_CLASS] && child->val <= keyword[T_VOID-T_CLASS])
					PARSER_PRINT(depth, "<keyword> %s </keyword>\n", child->val);
				else
					PARSER_PRINT(depth, "<identifier> %s </identifier>\n", child->val);
				child = child->next;
				PARSER_PRINT(depth, "<identifier> %s </identifier>\n", child->val);
				if(child->next)
					PARSER_PRINT(depth, "<symbol> , </symbol>\n");
			}
			--depth;
			PARSER_PRINT(depth, "</parameterList>");
			PARSER_PRINT(depth, "<symbol> ) </symbol>\n");
			break;
		case N_CLASSVARDEC:
			child = node->down;
			PARSER_PRINT(depth, "<classVarDec>");
			++depth;
			PARSER_PRINT(depth, "<keyword> %s </keyword>\n", child->val);
			child = child->next;
			if(child->val >= keyword[T_INT-T_CLASS] && child->val <= keyword[T_VOID-T_CLASS])
				PARSER_PRINT(depth, "<keyword> %s </keyword>\n", child->val);
			else
				PARSER_PRINT(depth, "<identifier> %s </identifier>\n", child->val);
			for(child = child->next; child; child = child->next)
				PARSER_PRINT(depth, "<identifier> %s </identifier>\n", child->val);
			PARSER_PRINT(depth, "<symbol> ; </symbol>\n");
			--depth;
			PARSER_PRINT(depth, "</classVarDec>");
			break;
		case N_VARDEC:
			child = node->down;
			PARSER_PRINT(depth, "<varDec>");
			++depth;
			PARSER_PRINT(depth, "<keyword> var </keyword>\n");
			child = child->next;
			if(child->val >= keyword[T_INT-T_CLASS] && child->val <= keyword[T_VOID-T_CLASS])
				PARSER_PRINT(depth, "<keyword> %s </keyword>\n", child->val);
			else
				PARSER_PRINT(depth, "<identifier> %s </identifier>\n", child->val);
			for(child = child->next; child; child = child->next)
				PARSER_PRINT(depth, "<identifier> %s </identifier>\n", child->val);
			PARSER_PRINT(depth, "<symbol> ; </symbol>\n");
			--depth;
			PARSER_PRINT(depth, "</varDec>");
			break;
		case N_STATEMENTS:
			PARSER_PRINT(depth, "<statements>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</statements>\n");
			break;
		case N_LETSTATEMENT:
			PARSER_PRINT(depth, "<letStatement>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</letStatement>\n");
			break;
		case N_IFSTATEMENT:
			PARSER_PRINT(depth, "<ifStatement>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</ifStatement>\n");
			break;
		case N_ELSESTATEMENT:
			/* NOTE if we hit this it means we're inside an N_IFSTATEMENT */
			PARSER_PRINT(depth, "<keyword> else </keyword>\n");
			PARSER_PRINT(depth, "<symbol> { </symbol>\n");
			ast_to_xml(node->next, depth);
			PARSER_PRINT(depth, "<symbol> } </symbol>\n");
			break;
		case N_WHILESTATEMENT:
			PARSER_PRINT(depth, "<whileStatement>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</whileStatement>\n");
			break;
		case N_DOSTATEMENT:
			PARSER_PRINT(depth, "<doStatement>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</doStatement>\n");
			break;
		case N_RETURNSTATEMENT:
			PARSER_PRINT(depth, "<returnStatement>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</returnStatement>\n");
			break;
		case N_EXPRESSIONLIST:
			PARSER_PRINT(depth, "<expressionList>\n");
			++depth;
			if(node->down) ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</expressionList>\n");
			break;
		case N_EXPRESSION:
			PARSER_PRINT(depth, "<expression>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</expression>\n");
			break;
		case N_TERM:
			PARSER_PRINT(depth, "<term>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</term>\n");
			break;
		case N_SUBROUTINECALL:
			PARSER_PRINT(depth, "<subroutineCall>\n");
			++depth;
			ast_to_xml(node->down, depth);
			--depth;
			PARSER_PRINT(depth, "</subroutineCall>\n");
			break;
		case N_OP:
			PARSER_PRINT(depth, "<symbol> %s </symbol>\n", node->val);
			break;
		case N_UNOP:
			PARSER_PRINT(depth, "<symbol> %s </symbol>\n", node->val);
			break;
		case N_INTCONST:
			PARSER_PRINT(depth, "<integerConstant> %s </integerConstant>\n", node->val);
			break;
		case N_STRINGCONST:
			PARSER_PRINT(depth, "<stringConstant> %s </stringConstant>\n", node->val);
			break;
		case N_KEYCONST:
			PARSER_PRINT(depth, "<keyword> %s </keyword>\n", node->val);
			break;
		case N_STORAGEQUALIFIER:
			PARSER_PRINT(depth, "<keyword> %s </keyword>\n", node->val);
			break;
		case N_TYPE:
			if(node->val >= keyword[T_INT-T_CLASS] && node->val <= keyword[T_VOID-T_CLASS])
				PARSER_PRINT(depth, "<keyword> %s </keyword>\n", node->val);
			else
				PARSER_PRINT(depth, "<identifier> %s </identifier>\n", node->val);
			break;
		case N_CLASSNAME:
			PARSER_PRINT(depth, "<identifier> %s </identifier>\n", node->val);
			break;
		case N_VARNAME:
			PARSER_PRINT(depth, "<identifier> %s </identifier>\n", node->val);
			break;
		case N_SUBROUTINENAME:
			PARSER_PRINT(depth, "<identifier> %s </identifier>\n", node->val);
			break;
		}
	}
}

/* NOTE our symbol table only needs to store variables not functions */
void sym_tab_build(Sym_tab *tab, AST_node *node) {
	AST_node *child;
	Sym symbol;
	size_t *tmp;

	switch(node->kind) {
	case N_CLASS:
		tab->name = node->val;
		node = node->down;
		if(node->kind != N_CLASSVARDEC)
			break;

		for(; node && node->kind == N_CLASSVARDEC; node = node->next) {
			child = node->down;
			assert(child->kind == N_STORAGEQUALIFIER);
			if(child->val == keyword[T_FIELD - T_CLASS]) {
				symbol.seg = S_THIS;
				tmp = &(tab->this_count);
			} else if(child->val == keyword[T_STATIC - T_CLASS]) {
				symbol.seg = S_STATIC;
				tmp = &(tab->static_count);
			}

			child = child->next;
			assert(child->kind == N_TYPE);
			symbol.type = child->val;
			while((child = child->next)) {
				symbol.pos = (*tmp)++;
				assert(child->kind == N_VARNAME);
				symbol.name = child->val;
				sym_tab_def(tab, &symbol);
			}
		}
		break;
	case N_SUBROUTINEDEC:
		node = node->down;
		assert(node->kind == N_STORAGEQUALIFIER);
		if(node->val == keyword[T_METHOD - T_CLASS]) {
			symbol = (Sym){
				.pos = tab->arg_count++,
				.seg = S_ARGUMENT,
				.type = keyword[T_THIS - T_CLASS],
				.name = keyword[T_THIS - T_CLASS],
			};
			sym_tab_def(tab, &symbol);
		}

		node = node->next->next;
		assert(node->kind == N_SUBROUTINENAME);
		tab->name = node->val;

		node = node->next;
		assert(node->kind == N_PARAMETERLIST);

		symbol.seg = S_ARGUMENT; /* the segment will be ARG for the duration of the loop */
		for(child = node->down; child; child = child->next) {
			symbol.pos = tab->arg_count++;
			assert(child->kind == N_TYPE);
			symbol.type = child->val;
			child = child->next;
			assert(child->kind == N_VARNAME);
			symbol.name = child->val;
			sym_tab_def(tab, &symbol);
		}

		node = node->next;
		assert(node->kind == N_SUBROUTINEBODY);
		node = node->down;
		if(node->kind != N_VARDEC)
			break;

		symbol.seg = S_LOCAL;
		for(; node && node->kind == N_VARDEC; node = node->next) {
			child = node->down;
			assert(child->kind == N_TYPE);
			symbol.type = child->val;
			while((child = child->next)) {
				symbol.pos = tab->local_count++;
				assert(child->kind == N_VARNAME);
				symbol.name = child->val;
				sym_tab_def(tab, &symbol);
			}
		}
		break;
	}
}

int main(int argc, char *argv[]) {
	if(argc == 1)
		return 1;

	atexit(cleanup);

	if(fmapopen(argv[1], O_RDONLY, &fm) < 0)
		error(1, 0, "%s: no such file or directory", argv[1]);
	fmapread(&fm);
	xmlout = fopen("xmlout", "w");
	astout = fopen("astout", "w");

	lexer.src = fm.buf;
	lexer.ptr = 0;

	depth = 1;
	STRPOOL_INIT(spool);
	SYM_TAB_INIT(classtab);
	SYM_TAB_INIT(functab);
	AST_INIT(ast);

	AST_node *root = parse();
	debug_parser(root, 0);
	sym_tab_build(&classtab, root);
	sym_tab_print(&classtab);
	fprintf(stderr, "\n#################\n\n");
	for(root = root->down; root->kind != N_SUBROUTINEDEC; root = root->next);
	sym_tab_build(&functab, root);
	sym_tab_print(&functab);
	
	return 0;
}
