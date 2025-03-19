#!/bin/bash

swipl --quiet --on-error=halt -t generate_spec src/spec_gen.pl | tee isa.md
