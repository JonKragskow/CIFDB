PROGRAM INITIALISEDB
    IMPLICIT NONE
    CHARACTER(LEN=500):: CIFListFile,NCIFChar
    CHARACTER(LEN=500),ALLOCATABLE::CIFList(:)
    INTEGER :: NCIFs,J

CALL GET_COMMAND_ARGUMENT(1,CIFListFile)

IF (ciflistfile=='-h') THEN
    WRITE(6,*) 'InitialiseDB (CIFListFile) (NumberOfCIFs)'
    WRITE(6,*) 'CIFListFile is a file containing a list of paths of the CIFs you want to add'
    STOP
END IF

CALL GET_COMMAND_ARGUMENT(2,NCIFChar)
READ(NCIFChar,*) NCIFs
ALLOCATE(CIFList(NCIFs))

OPEN(33, FILE = TRIM(ADJUSTL(CIFListFile)),STATUS = 'OLD')


DO J = 1,NCIFs
    READ(33,*) CIFList(J) 

    IF (J==1) THEN
        CALL SYSTEM('DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE.txt y y')!DOS`
        !CALL SYSTEM('./DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE y y')!Unix
        !WRITE(6,*) './DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE y y'!debug
    ELSE
        CALL SYSTEM('DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE.txt y n')!DOS
        !CALL SYSTEM('./DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE y n')!Unix
        !WRITE(6,*) './DBBuild '//trim(adjustl(CIFList(J)))//' CIFDATABASE y n'!debug
    END IF
END DO


END PROGRAM INITIALISEDB