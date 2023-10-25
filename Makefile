HC    = ghc
FLAGS = -O2 -threaded  -eventlog -rtsopts
#-Wall -Werror -rtsopts 

SRC1 	= ./src/Elem3D.hs
SRC2  	= ./src/Base.hs
SRC3  	= ./src/Files.hs
SRC4 	= ./src/Tone_map.hs
SRC5	= ./src/Figuras.hs

APP = ./src/simulacion.hs
APP0 = sim
APP3 = production
APP1 = p1
APP2 = p2
SRCP1 = ./practicas/Pract1.hs
SRCP2 = ./practicas/Pract2.hs

VPATH = ./src
BIN_DIR = ./bin

# all: $(APP) $(APP1)

# $(APP): $(SRC) $(SRC2) $(SRC3)
# 	$(HC) $(FLAGS) --make $< -package random -o $@
# 	strip $@

# $(APP1): $(SRCP1) $(SRCP12)
# 	$(HC) --make $< -package random -package hmatrix -o $@
# 	strip $@

sim:
$(APP0): $(APP) $(SRC1) $(SRC5)
	$(HC) $(FLAGS) --make -i$(VPATH) $< -package parallel  -package random -package split -package vector -o $@ 
	strip $@

production:
$(APP3): $(APP) $(SRC1) $(SRC5)
	$(HC) $(FLAGS) --make -i$(VPATH) $< -package random -o $@ && mv $(APP3) ./tmp
	strip $@


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
	rm -f $(APP0) $(APP1) $(APP2) $(APP3) $(BIN_DIR)/*.hi $(BIN_DIR)/*.o $(VPATH)/*.hi $(VPATH)/*.o ./*.eventlog ./tmp/$(APP3) ./tmp/*.zip ./tmp/*.bmp *.ppm *.bmp *.zip