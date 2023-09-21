HC    = ghc
FLAGS = -dynamic -O2 -threaded -Wall -Werror -rtsopts 

APP   = simulacion
SRC   = simulacion.hs
SRC2  = Base.hs
SRC3  = Files.hs

APP1 = p1
SRCP1 = Pract1.hs
SRCP12 = Elem3D.hs
all: $(APP) $(APP1)

$(APP): $(SRC) $(SRC2) $(SRC3)
	$(HC) $(FLAGS) --make $< -package random -o $@
	strip $@

$(APP1): $(SRCP1) $(SRCP12)
	$(HC) --make $< -package random -package hmatrix -o $@
	strip $@
	
clean:
	rm *.o *.hi $(APP) $(APP1)