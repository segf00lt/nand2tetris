#!/bin/sh

../jack 'case/Main'
diff expect/xml/Main xmlout || echo FAILED Main.sh
diff expect/ast/Main astout || echo FAILED Main.sh
rm xmlout astout
