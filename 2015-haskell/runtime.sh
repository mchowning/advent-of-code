#!/bin/bash

OUTPUT_DIR="haskell_output_14812908410984127"

if [[ $1 == *.hs ]]
then
    echo "ERROR: Only enter base name without .hs extension"
    exit 1
fi

stack ghc -- -O2 $1.hs -outputdir $OUTPUT_DIR

printf "script generated\nrunning script...\n\n"
time $1

printf "\nscript complete\ndeleting script...\n"
rm -rf $OUTPUT_DIR
rm $1

echo "done"
