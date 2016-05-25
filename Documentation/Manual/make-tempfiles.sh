#!/bin/sh

# Create Texinfo snippets from the documentation of exported symbols.

# I (Troels Henriksen) cribbed this script from SBCL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for more
# information.

if [ -z "$1" ]
then

    sbclsystem=$SBCL_PWD/../../src/runtime/sbcl
    sbclcore=$SBCL_PWD/../../output/sbcl.core
    if [ -e $sbclsystem ] && [ -e $sbclcore ]
    then
        SBCLRUNTIME="$sbclsystem --core $sbclcore"
    else
        SBCLRUNTIME="`which sbcl`"
    fi
else
    SBCLRUNTIME="$1"
fi

SBCL="$SBCLRUNTIME --noinform --no-sysinit --noprint --disable-debugger"

# Output directory.  This has to end with a slash (it's interpreted by
# Lisp's `pathname' function) or you lose.  This is normally set from
# Makefile.
DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"

echo /creating docstring snippets from SBCL=\'$SBCLRUNTIME\' for packages \'$PACKAGES\'
$SBCL <<EOF
(require :asdf)
(push (pathname "$SYSTEMSDIR") asdf:*central-registry*)
(asdf:oos 'asdf:load-op :cl-ppcre)
(asdf:oos 'asdf:load-op :mcclim)
(load "docstrings.lisp")
(defvar *output-dir* "$DOCSTRINGDIR")
(load "make-docstrings.lisp")
(sb-ext:quit)
EOF
