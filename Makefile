HC    = ghc
FLAGS = -dynamic -O2 -threaded -Wall -Werror -rtsopts 

APP   = simulacion
SRC   = simulacion.hs
SRC2  = Base.hs
all: $(APP)

$(APP): $(SRC) $(SRC2)
	$(HC) $(FLAGS) --make $< -package random -o $@
	strip $@

clean:
	rm *.o *.hi $(APP)