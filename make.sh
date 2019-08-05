#!/bin/sh

# https://guide.elm-lang.org/webapps/
# https://guide.elm-lang.org/interop/
echo "* Running elm make"
elm make src/Main.elm --output=target/main.js

echo "* Running sass to build milligram.css"
SASSAPP=./dart-sass/sass
[ -e $SASSAPP ] || echo $SASSAPP not installed, not building milligram.css
[ -e $SASSAPP ] && $SASSAPP ./milligram/src/milligram.sass >./milligram/milligram.css

echo "* Copying additional resources"
cp src-html/index.html target
cp ./milligram/milligram.css target

echo "* Done."
