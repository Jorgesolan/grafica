HC    = ghc
FLAGS = -O2 -funfolding-use-threshold=16 -fexcess-precision -optc-O3 -optc-ffast-math -threaded -eventlog -rtsopts -outputdir ./compil_files
#-Wall -Werror -rtsopts 
VPATH = ./src

SRC0 	= $(VPATH)/Funciones.hs
SRC1 	= $(VPATH)/Elem3D.hs
SRC3  	= $(VPATH)/Files.hs
SRC4 	= $(VPATH)/Tone_map.hs
SRC5	= $(VPATH)/Figuras.hs
SRC6	= $(VPATH)/PathTracer.hs
SRC7	= $(VPATH)/KdT.hs
SRC8	= $(VPATH)/PhotonMap.hs
SRC9	= $(VPATH)/Escena.hs

APP_0 = $(VPATH)/simulacion.hs
APP0 = simulacion

all: simulacion cargaKD
simulacion:
$(APP0): $(APP_0) $(SRC0) $(SRC1) $(SRC2) $(SRC3) $(SRC4) $(SRC6) $(SRC7) $(SRC8) $(SRC9)
	python3 inject.py
	$(HC) -static $(FLAGS) --make -i$(VPATH) $< -package random -static -o $@ 
# strip $@
	mv $(APP0) ./tmp/$(APP0)


clean:
	sudo rm -f ./tmp/$(APP0)* ./tmp/resultados/* ./compil_files/*.hi ./compil_files/*.o ./tmp/*.zip ./tmp/*.bmp ./tmp/*.ppm *.zip