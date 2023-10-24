#!/bin/bash
N=4
pids=()
for ((i = 1; i <= N; i++)); do
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

echo "All processes have finished."