#!/bin/bash
head -n 6 a1.ppm > output.ppm
for file in a*.ppm; do
    tail -n 1 "$file" | tr -d '\n' >> output.ppm
done
echo >> output.ppm