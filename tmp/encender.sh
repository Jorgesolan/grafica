#!/usr/bin/bash
for pc in {191..210} 
do
    echo "Encendiendo la máquina pc"
	/usr/local/etc/wake -y lab102-${pc}
done
