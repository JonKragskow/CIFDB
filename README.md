# CIFDB
A set of programs designed to create a personal bank of CIFs for a research group.



A set of programs designed to create a personal bank of CIFs for a research group. Creates a text file as a database which is human-readable. The interface to the program is the command line (terminal or cmd).

    InitialiseDB allows the user to import many CIFs at once to set up the database.
    DBBuild allows the user to add additional CIFs to their database
    CIFDBCheck allows the user to compare of a set of cell parameters (a, b, c, alpha,  beta, gamma, volume) to all of those in the database and list any potential matches

Written in Fortran 90/95 and provided as precompiled files and/or source code. If compiling from source then ensure the output file names are the same as the input file names (minus the .f95 extension). No extra libraries are required.

Please ensure all three programs are in the same directory as one another.

To run the program on a unix system type

    ./InitialiseDB (CIFListFile) (NumberOfCIFs)
        CIFListFile is a file containing a list of paths of the CIFs you want to add
    ./DBBuild (NewCIFFile) (DatabaseFile) <QuickAdd (y or n)>
    ./CIFDBCheck (DatabaseFile) (a) (b) (c) (alpha) (beta) (gamma) (volume)

or on a DOS system type

    InitialiseDB (CIFListFile) (NumberOfCIFs)
        CIFListFile is a file containing a list of paths of the CIFs you want to add
    DBBuild (NewCIFFile) (DatabaseFile) <QuickAdd (y or n)>
        QuickAdd command skips checking for similar CIFs
    CIFDBCheck (DatabaseFile) (a) (b) (c) (alpha) (beta) (gamma) (volume)

Items with brackets () are required arguments which should not be bracketed when entered into the program. Items with <> are optional and again should not be wrapped by <>.
