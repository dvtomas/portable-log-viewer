#!/bin/sh

# The app builds directly to the docs folder that is hosted at Github pages.
# This way we always have a fresh build available after push.

# https://guide.elm-lang.org/webapps/
# https://guide.elm-lang.org/interop/
echo "* Running elm make"
elm make --optimize src/Main.elm --output=docs/main.js

echo "* Running sass to build milligram.css"
SASSAPP=./dart-sass/sass
[ -e $SASSAPP ] || echo $SASSAPP not installed, not building milligram.css
[ -e $SASSAPP ] && $SASSAPP ./milligram/src/milligram.sass >./milligram/milligram.css

echo "* Copying additional resources"
cp src-html/index.html docs
cp ./milligram/milligram.css docs

echo "* Done."
