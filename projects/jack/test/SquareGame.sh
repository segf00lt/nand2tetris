#!/bin/sh

../jack 'case/SquareGame'
diff expect/xml/SquareGame xmlout || echo FAILED SquareGame.sh
diff expect/ast/SquareGame astout || echo FAILED SquareGame.sh
rm xmlout astout
