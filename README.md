# CIFDB
A set of programs designed to create a database of CIF cell parameters in the form of a human-readable text file for a research group or individual. The interface to the program is the command line (terminal or cmd).


.f90 files are source

Windows users please install minGW https://gcc.gnu.org/wiki/GFortranBinaries

Mac/Linux users please install gfortran/gcc via your package manager (apt, aptitude, homebrew). 

Compile with 
```
gfortran FILE.f90 -o FILE -ffree-line-length=3500
```
