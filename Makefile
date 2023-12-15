HC    = ghc
FLAGS = -O2 -funfolding-use-threshold=16 -fexcess-precision -optc-O3 -optc-ffast-math -threaded -eventlog -rtsopts
#-Wall -Werror -rtsopts 
VPATH = ./src
BIN_DIR = ./bin

SRC0 	= $(VPATH)/Funciones.hs
SRC1 	= $(VPATH)/Elem3D.hs
SRC3  	= $(VPATH)/Files.hs
SRC4 	= $(VPATH)/Tone_map.hs
SRC5	= $(VPATH)/Figuras.hs
SRC6	= $(VPATH)/PathTracer.hs

APP = $(VPATH)/simulacion.hs
APP0 = simulacion
APP3 = production
APP1 = p1
APP2 = p2
SRCP1 = ./practicas/Pract1.hs
SRCP2 = ./practicas/Pract2.hs


simulacion:
$(APP0): $(APP) $(SRC0) $(SRC1) $(SRC5) $(SRC6)
	$(HC) -static $(FLAGS) --make -i$(VPATH) $< -package random -static -o $@ 
	strip $@
	mv $(APP0) ./tmp

production:
$(APP3): $(APP) $(SRC1) $(SRC5)
	$(HC) $(FLAGS) --make -i$(VPATH) $< -package random -o $@
	strip $@
	mv $(APP3) ./tmp

p1:
$(APP1): $(SRCP1) $(SRC1)
	$(HC) --make $< -package random -package hmatrix -o $@
	strip $@

p2:

$(APP2): $(SRCP2) $(SRC1) $(SRC3) $(SRC4)
	$(HC) --make -i$(VPATH) $< -package random -package hmatrix -o $@
	strip $@

clean:
	-mv $(shell find . -name '*.o') $(BIN_DIR)
	-mv $(shell find . -name '*.hi') $(BIN_DIR)
	rm -f $(APP0) $(APP1) $(APP2) $(APP3) $(BIN_DIR)/*.hi $(BIN_DIR)/*.o $(VPATH)/*.hi $(VPATH)/*.o ./tmp/*.eventlog ./tmp/$(APP3) ./tmp/$(APP0) ./tmp/*.zip ./tmp/*.bmp ./tmp/*.ppm *.ppm *.bmp *.zip $(BIN_DIR) $(APP0)