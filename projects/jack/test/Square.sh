#!/bin/sh

../jack 'case/Square/Main.jack'
diff -Z expect/xml/Square/Main.xml xmlout || \
	(echo 'FAILED Square/Main.jack' && cp xmlout failure && exit 1)

../jack 'case/Square/Square.jack'
diff -Z expect/xml/Square/Square.xml xmlout || \
	(echo 'FAILED Square/Square.jack' && cp xmlout failure && exit 1)

../jack 'case/Square/SquareGame.jack'
diff -Z expect/xml/Square/SquareGame.xml xmlout || \
	(echo 'FAILED Square/SquareGame.jack' && cp xmlout failure && exit 1 )
