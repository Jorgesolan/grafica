#!/bin/bash
#crear ejec pilg
zip -r original_graf.zip ../../grafica/
mv original_graf.zip ..
scp ../original_graf.zip 192.168.0.252:~/
ssh -n 192.168.0.252 'unzip original_graf.zip;cd grafica;docker-compose up'
scp 192.168.0.252:~/grafica/tmp/simulacion_pilgor .
scp 192.168.0.252:~/grafica/tmp/cargaKD_pilgor .
ssh -n 192.168.0.252 'sudo rm -rf ./grafica original_graf.zip'
#crear ejec berl
cd ..
make all
cd tmp
mv simulacion simulacion_berlin
mv cargaKD cargaKD_berlin

#crear comprimido con todo para llevarlo a los entornos de ejec
zip -r compilados.zip ../../grafica/meshes ../../grafica/tmp
#copiar comprimido a central
scp compilados.zip a821259@central.cps.unizar.es:~/
ssh -n a821259@central.cps.unizar.es 'unzip compilados.zip; cp -r ./grafica/ /tmp; cd /tmp/grafica/tmp; mkdir resultados'