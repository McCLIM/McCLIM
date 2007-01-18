#!/bin/sh -e

# (Re-)Installs the top-level .asd files into an
# asdf:*central-registry* directory. Prompts before overwriting
# anything.

CENTRAL_REG="$1"

if [ -z "$CENTRAL_REG" ] ; then
    echo "USAGE: $0 central-registry-dir" 2>&1
    echo "	central-registry-dir is a directory where asdf looks for .asd files." 2>&1
    echo "	e.g. on SBCL, this could be ~/.sbcl/systems/" 2>&1
    exit 1
fi

cd "`dirname $0`"
for i in *.asd ; do
    if [ -e "$CENTRAL_REG"/"$i" ]; then
        echo -en "Warning: overwriting $CENTRAL_REG/$i with link to \n`pwd`/$i (press RET to continue)" 2>&1
        read
    fi
    ln -sf "`pwd`/$i" "$CENTRAL_REG"/"$i"
done