#!/bin/bash

set -e

echo "Generating 'isa.md'..."
swipl --quiet --on-error=halt -t generate_spec src/spec_gen.pl > isa.md

echo "Generating opcode tree graphs..."
swipl --quiet --on-error=halt -t print_dottrees src/optree.pl

# Process all .dot files in assets/graphs and generate .svg files in assets
echo "Converting GraphViz files to SVGs..."
for FILE in assets/graphs/*.dot; do
    BASENAME=$(basename "$FILE" .dot)
    echo "    Generating $BASENAME.svg..."
    dot -Tsvg:cairo -o "assets/$BASENAME.svg" "$FILE"
done

echo "Done!"
