#!/bin/bash

if [ ! $(which stack) ]
then
	echo "'stack' binary not found in your PATH"
	exit 1
fi

if [ ! $(which elm) ]
then
	echo "'elm' binary not found in your PATH"
	exit 1
fi

# nabbed from SO; cd to directory script lives in:
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PWD=$(pwd)
echo "Script pwd is: $PWD"

# build the haskell binary:
echo "=== building server binary ==="
cd server
stack install

# make client/build dir if not exists:
cd ../client
if [ ! -d build ]
then mkdir build
fi

# build elm JS and copy stuff into build dir:
cd src
echo "=== building client code ==="
elm make Main.elm --output ../build/app.js

echo "=== link static assets across ==="
cd ..

if [ ! -e "$PWD/build/static" ]
then ln -s "$PWD/src/static" "$PWD/build/static"
fi

if [ ! -e "$PWD/build/index.html" ]
then ln -s "$PWD/src/index.html" "$PWD/build/index.html"
fi

# run haskell binary (assume in PATH) and point at client build:
cd ..
echo "=== starting talklicker ==="
if [ $(which talklicker) ]
then talklicker --static client/build --db talklicker.json
else echo "'talklicker' binary not found in your PATH :("
fi