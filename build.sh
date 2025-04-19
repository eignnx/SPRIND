#!/bin/bash

set -e

TIMEFORMAT='[%Es]'

generate_svgs() {
    # Process all .dot files in assets/graphs and generate .svg files in assets
    DOT_FILES=(assets/graphs/*.dot)
    TOTAL_FILES=${#DOT_FILES[@]}
    COUNT=0

    for FILE in "${DOT_FILES[@]}"; do
        BASENAME=$(basename "$FILE" .dot)
        COUNT=$((COUNT + 1))
        # Check if any .dot files have changed since the last commit
        if git diff --quiet HEAD -- $FILE; then
            echo "    [$COUNT/$TOTAL_FILES] No change, skipping SVG generation for $BASENAME.dot..."
        else
            echo "    [$COUNT/$TOTAL_FILES] Generating $BASENAME.svg..."
            dot -Tsvg:cairo -o "assets/$BASENAME.svg" "$FILE"
        fi
    done
}


main() {
    echo "Generating markdown documents..."
    time swipl --quiet --on-error=halt -t generate_spec src/gen_spec.pl
    echo "Generating opcode tree graphs..."
    time swipl --quiet --on-error=halt -t print_dottrees src/optree.pl

    echo "Converting GraphViz files to SVGs..."
    time generate_svgs

    echo "Done!"
}

time main

