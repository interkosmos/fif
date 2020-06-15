FC     = gfortran
FFLAGS =
TARGET = compiler
SRC    = compiler.f90

.PHONY: clean

all: compiler

compiler: $(SRC)
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC)

clean:
	rm $(TARGET)
