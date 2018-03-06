FC  = flang
SRC = compiler.f90
BIN = compiler

all: $(BIN)

compiler: $(SRC)
	$(FC) -o $(BIN) $(SRC)

.PHONY: clean

clean:
	rm $(BIN)
