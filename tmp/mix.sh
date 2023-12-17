#!/bin/bash
etapasY=1
N=120
head -n 6 resultados/a0_0.ppm > output.ppm

for ((etapaY = 0; etapaY < etapasY; etapaY++)); do
  for ((i = 0; i < N; i++)); do
    fileBase="resultados/a${i}_${etapaY}"
    file="${fileBase}.ppm"
    tail -n 1 "$file" | tr -d '\n' >> output.ppm
    rm ${fileBase}* 
  done
done
echo >> output.ppm