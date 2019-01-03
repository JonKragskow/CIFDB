PROGRAM INITIALISEDB
    IMPLICIT NONE
    CHARACTER(LEN=500):: CIFListFile,NCIFChar
    CHARACTER(LEN=500),ALLOCATABLE::CIFList(:)
    INTEGER :: NCIFs,J

CALL GET_COMMAND_ARGUMENT(1,CIFListFile)

IF (ADJUSTL(TRIM(CifListFile))=='-h' .OR. ADJUSTL(TRIM(CifListFile))=='') THEN
    WRITE(6,*) 'InitialiseDB (CIFListFile) (NumberOfCIFs)'
    WRITE(6,*) 'CIFListFile is a file containing a list of paths of the CIFs you want to add'
    STOP
END IF

CALL GET_COMMAND_ARGUMENT(2,NCIFChar)
READ(NCIFChar,*) NCIFs
ALLOCATE(CIFList(NCIFs))

OPEN(66, FILE = 'CIFDATABASE.txt', STATUS = 'UNKNOWN')
    WRITE(66,'(I0)') 0
CLOSE(66)


OPEN(55, FILE = 'DBERRORS.txt', STATUS = 'UNKNOWN')
CLOSE(55)
OPEN(33, FILE = TRIM(ADJUSTL(CIFListFile)),STATUS = 'OLD')


    DO J = 1,NCIFs
        READ(33,'(A)') CIFList(J) 
            CALL SYSTEM('DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE.txt y y')!DOS`
            !CALL SYSTEM('./DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE.txt y y')!Unix
            !WRITE(6,*) './DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE.txt y y'!debug
    END DO

CLOSE(33)

WRITE(6,*) '****************************************************'
WRITE(6,*) 'Please Check DBERRORS.txt for any CIF related errors'
WRITE(6,*) '****************************************************'


END PROGRAM INITIALISEDB