#!/bin/bash

for file in tests/dotC/*.c; do
    echo "Testing : $file"
    ./glados < "$file"
    echo ""
done
