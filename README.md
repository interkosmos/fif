# fif
Fortran in Fortran: A FORTRAN I transpiler, written in Fortran 2008. Translates
FORTRAN I code to ANSI C, then calls Clang to compile it. Please see the
[lecture on nostalgic programming in Fortran](http://www.contrib.andrew.cmu.edu/~wmaynes/lect/lecture01.pdf)
for further information.

## Build
Use the Makefile to build the transpiler:
```
$ make
```
Or run Flang directly:
```
$ flang -o compiler compiler.f90
```
At the moment, GNU Fortran is not supported.

## Compile
Compile your FORTRAN I code with:
```
$ ./compiler <input> <output>
```
For instance:
```
$ ./compiler average.f average
```
This outputs the translated C code to `average.c` and runs the C compiler to
build it. You can change the used C compiler in `compiler.f90` (line 1433).
The default is `clang`.

## Copyright
Copyright © 2018, Brandon Bohrer. Licenced under MIT.
