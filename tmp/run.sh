#!/bin/bash
N=10
pix=1000
piy=1000
etapasY=1
etapasX=1  # You can change this value to the desired number of iterations per barrier
pids=()
binName="./simulacion"
# Calculate the number of iterations per barrier
iterations_per_barrier=$((N / etapasY))

for ((etapaY = 0; etapaY < etapasY; etapaY++)); do
  for ((etapaX = 0; etapaX < etapasX; etapaX++)); do
    for ((i = 0; i < N; i++)); do
      echo "lanzando proc ${i}"
      if ((i == (N - 1))); then
        ${binName} ${i} ${etapaY} ${etapaX}
      else
        ${binName} ${i} ${etapaY} ${etapaX}  &
        pids+=($!)
      fi
    
    done
    # Wait for the background processes to finish before proceeding to the next etapa
    for pid in "${pids[@]}"; do
      wait $pid
    done
    # Clear the pids array for the next etapa
    pids=()
  done
done


head -n 6 a0_0_0.ppm > output.ppm
for ((etapaY = 0; etapaY < etapasY; etapaY++)); do
  for ((i = 0; i < N; i++)); do
    python3 parseXEpochs.py ${i} ${etapaY} ${etapasX} ${pix} ${piy} ${etapasY} ${N}
    fileBase="a${i}_${etapaY}"
    file="${fileBase}.ppm"
    tail -n 1 "$file" | tr -d '\n' >> output.ppm
    # rm ${fileBase}* 
  done
done
echo >> output.ppm

echo "All processes have finished."