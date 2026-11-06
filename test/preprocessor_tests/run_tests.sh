#!/bin/bash

# Create a directory for results if it doesn't exist
mkdir -p preprocessed_results

for file in *.c; do
    echo "Processing $file..."
    gcc -P "$file" -o "preprocessed_results/${file%.c}.preprocessed"
done

echo "All tests processed."
