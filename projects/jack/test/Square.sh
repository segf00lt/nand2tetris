#!/bin/sh

../jack 'case/Square'
diff expect/xml/Square xmlout || echo FAILED Square.sh
diff expect/ast/Square astout || echo FAILED Square.sh
rm xmlout astout
