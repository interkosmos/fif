FC  = flang
SRC = compiler.f90
BIN = compiler

all: compiler

compiler: $(SRC)
	$(FC) -o $(BIN) $(SRC)

.PHONY: clean

clean:
	rm $(BIN)
