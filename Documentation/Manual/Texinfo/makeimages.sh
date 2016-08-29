#!/bin/sh

# Go through a list of image files and make sure that a version of
# each image exists in every format specified by a list of file
# types. Uses ImageMagick. If first argument is -e, to not convert,
# but print a list of the files that are supposed to exist after the
# script has run.
if [ "$1" == "-e" ]; then
    NOCREATE="true"
    IMAGES="$2"
    IMAGETARGETTYPES="$3"
else
    IMAGES=$1
    IMAGETARGETTYPES=$2
fi

for FILE in $IMAGES; do
    if [ -f "$FILE" ]; then
        RAWFILE=${FILE%.*}
        for TYPE in $IMAGETARGETTYPES; do
            FILETOMAKE=$RAWFILE.$TYPE
            if [ "$NOCREATE" == "true" ]; then
                echo $FILETOMAKE
            else
                if [ ! -f "$FILETOMAKE" ]; then
                    convert $FILE $FILETOMAKE
                fi
            fi
        done
    else
        echo The file $FILE does not exist.
    fi
done