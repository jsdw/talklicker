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

rsync -av "$(pwd)/src/static/" "$(pwd)/build/static"
rsync "$(pwd)/src/index.html" "$(pwd)/build/index.html"
