#!/bin/bash
N=8
etapas=1  # You can change this value to the desired number of iterations per barrier
pids=()

# Calculate the number of iterations per barrier
iterations_per_barrier=$((N / etapas))

for ((etapa = 0; etapa < etapas; etapa++)); do
  for ((i = etapa * N + 1; i <= (etapa + 1) * N; i++)); do
    echo "lanzando proc ${i}"
    if ((i == (etapa + 1) * N)); then
      ./simulacion +RTS -l -s -RTS ${i}
    else
      ./simulacion +RTS -l -s -RTS ${i} &
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


head -n 6 a1.ppm > output.ppm
for ((i = 1; i <= N * etapas; i++)); do
    file="a${i}.ppm"
    tail -n 1 "$file" | tr -d '\n' >> output.ppm
    rm $file
done
echo >> output.ppm

echo "All processes have finished."