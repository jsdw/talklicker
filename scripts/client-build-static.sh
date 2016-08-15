#!/bin/bash

# exit on first sign of error:
set -e

# nabbed from SO; cd to directory project lives in:
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ..

echo "Project source is: $(pwd)"

# make client/build dir if not exists:
cd client
if [ ! -d build ]
then mkdir build
fi


if [ ! -e "$(pwd)/build/static" ]
then
	ln -s "$(pwd)/src/static" "$(pwd)/build/static"
	echo "Linking static dir"
fi

if [ ! -e "$(pwd)/build/index.html" ]
then
	ln -s "$(pwd)/src/index.html" "$(pwd)/build/index.html"
	echo "Linking index.html"
fi