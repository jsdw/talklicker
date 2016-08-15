#!/bin/bash

# exit on first sign of error:
set -e

# nabbed from SO; cd to directory script lives in:
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ..
echo "Script pwd is: $(pwd)"

# test some prerequisities
if [ ! $(which stack) ]
then
	echo "'stack' binary not found in your PATH"
	exit 1
fi

# build the haskell binary:
echo "=== building server binary ==="
cd server
stack install

# run haskell binary (assume in PATH) and point at client build:
cd ..
echo "=== starting talklicker ==="
if [ $(which talklicker) ]
then talklicker --static client/build --db server/talklicker.json
else echo "'talklicker' binary not found in your PATH :("
fi