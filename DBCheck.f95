PROGRAM CIFDBCheck
IMPLICIT NONE
CHARACTER(LEN=100) :: INaChar, INbChar, INcChar, INAlphaChar, INBetaChar, INGammaChar, INVolumeChar,DatabaseFile !InCollectionDay, InCollectionMonth, InCollectionYear
CHARACTER(LEN = 100), ALLOCATABLE ::  DBCollectionCode(:), DBCollectionDate(:), DBCollectionEntity(:), DBSystematicName(:)
INTEGER :: NDatabaseEntries
REAL(KIND = 8) :: INa, INb, INc, INAlpha, INBeta, INGamma, INVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol
REAL(KIND = 8), ALLOCATABLE :: DBa(:), DBb(:), DBc(:), DBAlpha(:), DBBeta(:),DBGamma(:), DBVolume(:)

call GET_COMMAND_ARGUMENT(1,DatabaseFile)

IF(TRIM(ADJUSTL(DatabaseFile)) == '-h' .or. TRIM(ADJUSTL(DatabaseFile)) == '') THEN
               WRITE(6,*) 'CIFDBCheck (DatabaseFile) (a) (b) (c) (alpha) (beta) (gamma) (volume)'
                STOP
END IF

!Read Inputs
call GET_COMMAND_ARGUMENT(2,INaChar)
call GET_COMMAND_ARGUMENT(3,INbChar)
call GET_COMMAND_ARGUMENT(4,INcChar)
call GET_COMMAND_ARGUMENT(5,INAlphaChar)
call GET_COMMAND_ARGUMENT(6,INBetaChar)
call GET_COMMAND_ARGUMENT(7,INGammaChar)
call GET_COMMAND_ARGUMENT(8,INVolumeChar)


READ(INaChar,*) Ina
READ(INbChar,*) Inb
READ(INcChar,*) Inc
READ(INAlphaChar,*) Inalpha
READ(INBetaChar,*) Inbeta
READ(INGammaChar,*) InGamma
READ(INVolumeChar,*) InVolume


CALL SelfPromo

CALL ReadNEntries(DatabaseFile, NDatabaseEntries)

ALLOCATE(DBCollectionCode(NDatabaseEntries), DBCollectionDate(NDatabaseEntries), DBCollectionEntity(NDatabaseEntries), DBSystematicName(NDatabaseEntries), DBa(NDatabaseEntries), DBb(NDatabaseEntries), DBc(NDatabaseEntries), DBAlpha(NDatabaseEntries), DBBeta(NDatabaseEntries), DBGamma(NDatabaseEntries), DBVolume(NDatabaseEntries))

CALL ReadDatabase(DatabaseFile, NDatabaseEntries, DBCollectionCode, DBCollectionDate, DBCollectionEntity, DBSystematicName, DBa, DBb, DBc, DBAlpha, DBBeta, DBGamma, DBVolume)

CALL SetTolerances(aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol)

CALL CheckAndWrite( NDatabaseEntries, INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol, DBa, DBb, DBc, DBAlpha, DBbeta, DBGamma, DBVolume)

CONTAINS

SUBROUTINE SelfPromo

WRITE(6,*)'                           /\             '
WRITE(6,*)'                          /  \            '
WRITE(6,*)'                         /    \           '
WRITE(6,*)'                        /      \          '
WRITE(6,*)'                       /        \         '
WRITE(6,*)'                      /          \        '
WRITE(6,*)'                     /            \       '
WRITE(6,*)'                    /              \      '
WRITE(6,*)'                   /                \     '
WRITE(6,*)'                  /                  \    '
WRITE(6,*)'                 /       CIFDB        \   '
WRITE(6,*)'                /     DBCheck V3.0     \  '
WRITE(6,*)'               /                        \ '
WRITE(6,*)'               \  By Jon G. C. Kragskow / '
WRITE(6,*)'                \  kragskow.com/cifdb  /  '
WRITE(6,*)'                 \                    /   '
WRITE(6,*)'                  \                  /    '
WRITE(6,*)'                   \                /     '
WRITE(6,*)'                    \              /      '
WRITE(6,*)'                     \            /       '
WRITE(6,*)'                      \          /        '
WRITE(6,*)'                       \        /         '
WRITE(6,*)'                        \      /          '
WRITE(6,*)'                         \    /           '
WRITE(6,*)'                          \  /            '
WRITE(6,*)'                           \/             '

END SUBROUTINE SelfPromo

SUBROUTINE ReadNEntries(DatabaseFile, NDatabaseEntries)
IMPLICIT NONE
INTEGER :: NDatabaseEntries
CHARACTER(LEN = 100) :: DatabaseFile


OPEN(66, FILE = DatabaseFile, STATUS = 'OLD')
    READ(66,*) NDatabaseEntries
CLOSE(66)

END SUBROUTINE ReadNEntries

SUBROUTINE ReadDatabase(DatabaseFile, NDatabaseEntries, DBCollectionCode, DBCollectionDate, DBCollectionEntity, DBSystematicName, DBa, DBb, DBc, DBAlpha, DBBeta, DBGamma, DBVolume)
IMPLICIT NONE
CHARACTER(LEN = 250) :: BIGLINE
CHARACTER(LEN = 100) :: DatabaseFile, DBCollectionCode(:), DBCollectionEntity(:), DBSystematicName(:), DBCollectionDate(:)
REAL(KIND = 8) ::DBa(:), DBb(:), DBc(:), DBAlpha(:), DBBeta(:), DBGamma(:), DBVolume(:)
INTEGER :: DBE, NDatabaseEntries

OPEN(66, FILE = DatabaseFile, STATUS = 'OLD')

    READ(66,*) NDatabaseEntries  !Read Number of Database Entries
        READ(66,*) !READ BLANK
        READ(66,*) !READ BLANK
        READ(66,*) !READ BLANK

    
    !Loop over number of entries and read information
    !Hacky way of reading in names with commas out of the databse file
        DO DBE = 1,NDatabaseEntries
            READ(66,'(A)') BIGLINE
            BIGLINE = Replace_Text(BIGLINE,',','~~') !Replace commas with ~~
            READ(BIGLINE,*) DBCollectionCode(DBE), DBCollectionDate(DBE),  DBCollectionEntity(DBE), DBSystematicName(DBE), DBa(DBE), DBb(DBE), DBc(DBE), DBAlpha(DBE), DBBeta(DBE), DBGamma(DBE), DBVolume(DBE)
            DBSystematicName(DBE) = ADJUSTL(TRIM(Replace_Text(DBSystematicName(DBE),'~~',','))) !Replace ~~ with commas
    
    END DO
CLOSE(66)

END SUBROUTINE ReadDatabase

SUBROUTINE SetTolerances(aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol)
IMPLICIT NONE
CHARACTER(LEN = 5) :: YoN
REAL(KIND = 8) :: DefaultTol, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol

!Allow user to set different tolerance or use global tolerance
DefaultTol = 5.0_8

aTol = DefaultTol
bTol = DefaultTol
cTol = DefaultTol
alphaTol = DefaultTol
betaTol = DefaultTol
gammaTol = DefaultTol
VolTol = DefaultTol

WRITE(6,*)
WRITE(6,*) ' Tolerance Is +/-5% for all parameters'
WRITE(6,*) 'Would you like to change this? (y/n)'
WRITE(6,*)
READ(5,*) YoN
DO WHILE (YoN/='y' .AND. YoN/='n')
    WRITE(6,*)
    WRITE(6,*) 'Lets try that again Chief'
    WRITE(6,*) 'Would you like to change this? (y/n)'
    READ(5,*) YoN
END DO

IF (YoN == 'y') THEN
    WRITE(6,*) 'Please input tolerance percentage as an integer in this order'
    WRITE(6,*) 'a b c alpha beta gamma volume'
    READ(5,*) aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol

    aTol = aTol/100.0_8
    bTol = bTol/100.0_8
    cTol = cTol/100.0_8
    alphaTol = alphaTol/100.0_8
    betaTol = betaTol/100.0_8
    gammaTol = gammaTol/100.0_8
    VolTol = VolTol/100.0_8
END IF


END SUBROUTINE SetTolerances


FUNCTION RangeFall(Val,DBVal, Tol) RESULT(out)

REAL(KIND = 8) :: Val, DBVal, Tol
CHARACTER(LEN=1) :: out

IF (Val >= DBVal + DBVal*Tol) THEN
    out = 'n'

ELSEIF (Val <= DBVal - DBVal*Tol) THEN
    out = 'n'

ELSE
    out = 'y'

END IF

END FUNCTION RangeFall

SUBROUTINE CheckAndWrite(NDatabaseEntries, INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol, DBa, DBb, DBc, DBAlpha, DBbeta, DBGamma, DBVolume)
IMPLICIT NONE
INTEGER :: DBE, NDatabaseEntries, SimilarityFlag
INTEGER, ALLOCATABLE :: SimilarityCount(:,:)
REAL(KIND = 8) :: INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol
REAL(KIND = 8) :: DBa(:), DBb(:), DBc(:), DBAlpha(:), DBBeta(:), DBGamma(:), DBVolume(:)
CHARACTER(LEN = 5) :: QuickFlag

ALLOCATE(SimilarityCount(NDatabaseEntries,7)) !SimilarityCount(a,b,c,alpha,beta,gamma,volume)

DO DBE = 1, NDatabaseEntries

    IF (RangeFall(INa, DBa(DBE),aTol) == 'y') SimilarityCount(DBE,1) = 1

    IF (RangeFall(INb, DBb(DBE),bTol) == 'y') SimilarityCount(DBE,2) = 1

    IF (RangeFall(INc, DBc(DBE),cTol) == 'y') SimilarityCount(DBE,3) = 1

    IF (RangeFall(INalpha, DBalpha(DBE),alphaTol) == 'y') SimilarityCount(DBE,4) = 1
        
    IF (RangeFall(INBeta, DBBeta(DBE),betaTol) == 'y') SimilarityCount(DBE,5) = 1

    IF (RangeFall(INGamma, DBGamma(DBE),gammaTol) == 'y') SimilarityCount(DBE,6) = 1

    IF (RangeFall(INVolume, DBVolume(DBE),VolTol) == 'y') SimilarityCount(DBE,7) = 1

END DO

!Add on Similarities
DO DBE = 1, NDatabaseEntries

    IF (SimilarityCount(DBE,1) == 1 .AND. SimilarityCount(DBE,2) == 1 .AND. SimilarityCount(DBE,3) == 1 .AND. SimilarityCount(DBE,4) == 1 .AND. SimilarityCount(DBE,5) == 1 .AND. SimilarityCount(DBE,6) == 1 .AND. SimilarityCount(DBE,7) == 1) SimilarityFlag = 1

END DO

IF ( quickflag /= 'y') THEN
    IF (SimilarityFlag > 0) THEN

        WRITE(6,*) 'Similarities:' 
        WRITE(6,*) 
        
        DO DBE = 1, NDatabaseEntries
        IF (SimilarityCount(DBE,1) == 1 .AND. SimilarityCount(DBE,2) == 1 .AND. SimilarityCount(DBE,3) == 1 .AND. SimilarityCount(DBE,4) == 1 .AND. SimilarityCount(DBE,5) == 1 .AND. SimilarityCount(DBE,6) == 1 .AND. SimilarityCount(DBE,7) == 1) THEN
            WRITE(6,'(A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F8.3, A, A)') 'a, b, c, alpha, beta, gamma, volume (', Ina,', ', Inb,', ', Inc,', ', InAlpha,', ', InBeta,', ', INGamma,', ', InVolume, ') are close to that of ', ADJUSTL(TRIM(DBCollectionCode(DBE)))
            WRITE(6,'(A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F8.3, A)') 'a, b, c, alpha, beta, gamma, volume (', DBa(DBE),', ', DBb(DBE),', ', DBc(DBE),', ', DBAlpha(DBE),', ', DBBeta(DBE),', ', DBGamma(DBE),', ', DBVolume(DBE),')'
            WRITE(6,*)
        END IF

    END DO
                
    ELSE
        WRITE(6,*) 
        WRITE(6,*) 'No Similarities'
        WRITE(6,*) 
    END IF
END IF

END SUBROUTINE CheckAndWrite



FUNCTION Replace_Text (s,old,new)  RESULT(outs)

!http://fortranwiki.org/fortran/show/String_Functions

CHARACTER(*)        :: s,old,new
CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
INTEGER             :: i, lenold, lennew

outs = s ; lenold = LEN_TRIM(old) ; lennew = LEN_TRIM(new)
DO WHILE(.TRUE.)
   i = INDEX(outs,old(:lenold))  !gives item number for old text in string
   IF (i == 0) EXIT !if string not found then leave loop
   outs = outs(:i-1) // new(:lennew) // outs(i+lenold:) !concatenate new text inbetween pieces of old text
END DO
END FUNCTION Replace_Text



END PROGRAM CIFDBCheck


