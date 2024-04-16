<h1 align="center"> Path Tracing y Photon Mapping en Haskell </h1>

<p align="center">
    <p align="center">
    <a href="https://github.com/Jorgesolan"><strong>Jorge Solán</strong></a>
    &nbsp;&nbsp;&nbsp;&nbsp;
    <a href="https://github.com/FranciscoJavierPizarro"><strong>Francisco Javier Pizarro</strong></a>
  </p>
</p>

<div align="center">
  <img src="/Images/hask.jpeg" width="250" height="250"/>
  <img src="/Images/Parrot.jpeg" width="500" height="250"/>
  <img src="/Images/phot.jpeg" width="250" height="250"/>
</div>

## Introducción

Trabajo desarrollado en la asignatura de informática gráfica de cuarto de carrera del grado de Ingeniería Informática en la Universidad de Zaragoza-Unizar.

### Características principales

El trabajo consiste en recrear escenas a partir de dos renders diferentes, Photon Mapping y Path Tracing.

La escena se define en el fichero correspondiente "Escena.hs", primero se define todo lo relacionado a la imágen final, número de fotones lanzados si estamos en photon mapping, intensisdad de la luz, píxeles y aspect ratio de la imagen final, además de las muestras por píxel y si se quiere realizar concurrente o distribuido, parámetros extra para la partición. 
Seguidamente, se pueden definir diferentes geometrias como esferas, planos, tríangulos o rectángulos, además de definir los puntos de luz y la cámara. Si se quiere utilizar mallas de triangulos con .obj es necesario indicarlo en el fichero "objs.hs".

Toda figura viene definida por sus puntos característicos, sus normales, tripleta de cómo se comporta con la luz (Difuso, Refractante o Especular), y se le puede indicar la textura que tenga el objeto, da igual el que sea.

Solo con esto ya se puede crear algunos renders simples, sin embargo este trabajo, además de la complicación de realizarlo en Haskell, se añadieron varios apartados adicionales, entre las más destacables entrarían las siguientes:


<div align='center'>
<h4>Brdf Phong con alpha modificable</h4>
  <img src="/Images/phong.jpeg" width="400" height="400"/>
  <h4>Bump mapping a partir de una textura</h4>
  <img src="/Images/wall.jpeg" width="400" height="400"/>
  <h4>Niebla homogénea con partículas modificables en color y cantidad</h4>
  <img src="/Images/niebla.jpeg" width="400" height="400"/>
</div><br>
Todo lo relacionado al trabajo queda explicado en su totalidad en las memorias correspondietes a los dos renders ubicadas en este mismo proyecto.

## Ejecución


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
<div align='center'>
<br>
<a href='https://render-haskell.duckdns.org/'> <strong>Documentación</strong></a>