#!/bin/bash

for pc in {191..210} 
do
    echo "Apagando la máquina $pc"
    /usr/local/etc/shutdown.sh -y lab102-${pc} &
done