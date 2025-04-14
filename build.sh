#!/bin/bash

set -e

TIMEFORMAT='[%Es]'

echo "Generating markdown documents..."
swipl --quiet --on-error=halt -f none -t generate_spec src/gen_spec.pl
echo "Generating opcode tree graphs..."
swipl --quiet --on-error=halt -f none -t print_dottrees src/optree.pl

generate_svgs () {
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
}

# Check if any .dot files have changed since the last commit
if git diff --quiet HEAD -- assets/graphs/*.dot && [[ $1 != '--regen' ]]; then
    echo "No changes in .dot files. Skipping SVG generation."
else
	generate_svgs
fi

echo "Done!"
