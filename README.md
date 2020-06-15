# fif
FORTRAN in Fortran: A FORTRAN I transpiler, written in Fortran 2008. Translates
FORTRAN I code to ANSI C. Please see the
[lecture on nostalgic programming in Fortran](http://cyber.dabamos.de/programming/fortran/transcompiler/lecture01.pdf)
for further information. The details of the language are specified in the
[Programmer’s Reference Manual](https://www.fortran.com/FortranForTheIBM704.pdf)
from 1956.

## Build Instructions
Use the provided `Makefile` to build the transpiler:

```
$ make
```

Or, run GNU Fortran directly:

```
$ gfortran -o compiler compiler.f90
```

## Compilation
Compile your FORTRAN I code with:

```
$ ./compiler <input> <output>
```

For instance:

```
$ ./compiler average.f average
$ ./average
3
10.0 12.2 17.8
 10.000
 12.200
 17.800
AVERAGE:  13.333
```

## Copyright
Copyright © 2018, Brandon Bohrer. Licenced under MIT.
