#!/bin/bash
nstart=$1
nend=$2
N=$3
pix=4176
piy=4176
etapasY=1
etapasX=8
pids=()
binName="./simulacion_${4}"
machine=$(hostname)

# Calculate the number of iterations per barrier
iterations_per_barrier=$((N / etapasY))

for ((etapaY = 0; etapaY < etapasY; etapaY++)); do
  for ((etapaX = 0; etapaX < etapasX; etapaX++)); do
    for ((i = nstart; i < nend; i++)); do
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

mkdir results
for ((etapaY = 0; etapaY < etapasY; etapaY++)); do
  for ((i = nstart; i < nend; i++)); do
    python3 parseXEpochs.py ${i} ${etapaY} ${etapasX} ${pix} ${piy} ${etapasY} ${N}
    fileBase="a${i}_${etapaY}"
    file="${fileBase}.ppm"
    mv ${file} results
    rm ${fileBase}_* 
  done
done

scp -o StrictHostKeyChecking=no ./results/* central:/tmp/grafica/tmp/resultados