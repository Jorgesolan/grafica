Aqui comienza la putisima historia donde dos locos hicieron un render con haskell :D

# Pasos para poder ejecutar algo

Es necesario contar con las herramientas `cabal`, `ghc`, `make`, `bash/shell`, `python` (y para despliegues distribuidos `ruby`)

El primer paso es instalar las dependencias del proyecto haskell.

```bash
cabal install JuicyPixels

cabal install random

cabal install kdt

cabal install erf

cabal install binary

cabal install containers

cabal install dlist
```

Para ejecutar el proyecto en *local* lanzar el siguiente comando desde la carpeta raiz:`make clean && make simulacion && cd ./tmp && ./simulacion  && ./run.sh   && cd .. && convert ./tmp/output.ppm a.bmp`.

Los ficheros que se deberían modificar entre ejecuciones principalmente son `/src/Escena.hs`, para modificar los elementos(figuras básicas/luces) de la escena, así como sus propiedades(algunas propiedades deben ser modificadas en `/tmp/run.sh`), el fichero `objs.hs` cuenta con el listado de los elementos que se importan de archivos objs. Para cambiar el modo de funcionamiento de Path Tracing a Photon Mapping y viceversa modificar `/etc/simulacionOrg.hs`. Es posible modificar funciones en el interior de `/src/PathTracer.hs` y `/src/PhotonMapper.hs`, principalmente para añadir/quitar la niebla.

En caso de tener cualquier duda relativa a como se debe ejecutar esto, no duden en contactarnos(dado que no es simplemente darle a un botón y puede resultar complejo).

La documentación se genera mediante el comando:

>haddock -o docs/ --quickjump --html --hyperlinked-source src/{Elem3D,Escena,Figuras,Files,Funciones,KdT,PathTracer,PhotonMap,simulacion,Tone_map}.hs