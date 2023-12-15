#!/bin/bash
N=72
etapas=30  # You can change this value to the desired number of iterations per barrier
pids=()

# Calculate the number of iterations per barrier
iterations_per_barrier=$((N / etapas))

for ((etapa = 0; etapa < etapas; etapa++)); do
  for ((i = 0; i < N; i++)); do
    echo "lanzando proc ${i}"
    if ((i == (N - 1))); then
      ./simulacion_pilgor ${i} ${etapa}
    else
      ./simulacion_pilgor ${i} ${etapa} &
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


head -n 6 a0_0.ppm > output.ppm
for ((etapa = 0; etapa < etapas; etapa++)); do
  for ((i = 0; i < N; i++)); do
    file="a${i}_${etapa}.ppm"
    tail -n 1 "$file" | tr -d '\n' >> output.ppm
    rm $file
  done
done
echo >> output.ppm

echo "All processes have finished."