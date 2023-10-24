#!/bin/bash
N=32
pids=()
for ((i = 1; i <= N; i++)); do
  echo "lanzando proc ${i}"
  if [ $i -eq $N ]; then
    ./production ${i}
  else
    ./production  ${i} &
    pids+=($!)
  fi
done

for pid in "${pids[@]}"; do
  wait $pid
done

head -n 6 a1.ppm > output.ppm
for ((i = 1; i <= N; i++)); do
    file="a${i}.ppm"
    tail -n 1 "$file" | tr -d '\n' >> output.ppm
done
echo >> output.ppm

echo "All processes have finished."