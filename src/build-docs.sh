#!/bin/bash

set -e

echo "Generating 'isa.md'..."
swipl --quiet --on-error=halt -t generate_spec src/spec_gen.pl > isa.md

echo "Generating opcode tree graphs..."
swipl --quiet --on-error=halt -t print_dottrees src/optree.pl

# Process all .dot files in assets/graphs and generate .svg files in assets
echo "Converting GraphViz files to SVGs..."
DOT_FILES=(assets/graphs/*.dot)
TOTAL_FILES=${#DOT_FILES[@]}
COUNT=0

for FILE in "${DOT_FILES[@]}"; do
    COUNT=$((COUNT + 1))
    BASENAME=$(basename "$FILE" .dot)
    echo "    [$COUNT/$TOTAL_FILES] Generating $BASENAME.svg..."
    dot -Tsvg:cairo -o "assets/$BASENAME.svg" "$FILE"
done

echo "Done!"
