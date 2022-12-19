#!/bin/sh

../jack 'case/ArrayTest/Main.jack'
diff -Z expect/ArrayTest/xml/Main.xml xmlout || \
	(echo 'XML FAILED ArrayTest/Main.jack' && cp xmlout failure && exit 1)

diff -Z expect/ArrayTest/sym/Main symout || \
	(echo 'SYMBOL TABLE FAILED ArrayTest/Main.jack' && cp symout failure && exit 1)

diff -Z expect/ArrayTest/ast/Main astout || \
	(echo 'AST FAILED ArrayTest/Main.jack' && cp astout failure && exit 1)
