FC     = gfortran7
CFLAGS = -Wl,-rpath=/usr/local/lib/gcc7/
SRC    = compiler.f90
BIN    = compiler

all: compiler

compiler: $(SRC)
	$(FC) $(CFLAGS) -o $(BIN) $(SRC)

.PHONY: clean

clean:
	rm $(BIN)
