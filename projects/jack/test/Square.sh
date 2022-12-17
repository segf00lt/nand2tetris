#!/bin/sh

../jack 'case/Square/Main.jack'
diff -Z expect/Square/xml/Main.xml xmlout || \
	(echo 'XML FAILED Square/Main.jack' && cp xmlout failure && exit 1)

diff -Z expect/Square/sym/Main symout || \
	(echo 'SYMBOL TABLE FAILED Square/Main.jack' && cp symout failure && exit 1)

diff -Z expect/Square/ast/Main astout || \
	(echo 'AST FAILED Square/Main.jack' && cp astout failure && exit 1)

../jack 'case/Square/Square.jack'
diff -Z expect/Square/xml/Square.xml xmlout || \
	(echo 'XML FAILED Square/Square.jack' && cp xmlout failure && exit 1)

diff -Z expect/Square/sym/Square symout || \
	(echo 'SYMBOL TABLE FAILED Square/Square.jack' && cp symout failure && exit 1)

diff -Z expect/Square/ast/Square astout || \
	(echo 'AST FAILED Square/Square.jack' && cp astout failure && exit 1)

../jack 'case/Square/SquareGame.jack'
diff -Z expect/Square/xml/SquareGame.xml xmlout || \
	(echo 'XML FAILED Square/SquareGame.jack' && cp xmlout failure && exit 1)

diff -Z expect/Square/sym/SquareGame symout || \
	(echo 'SYMBOL TABLE FAILED Square/SquareGame.jack' && cp symout failure && exit 1)

diff -Z expect/Square/ast/SquareGame astout || \
	(echo 'AST FAILED Square/SquareGame.jack' && cp astout failure && exit 1)
