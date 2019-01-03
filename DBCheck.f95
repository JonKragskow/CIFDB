PROGRAM CIFDBCheck
IMPLICIT NONE
CHARACTER(LEN=100) :: INaChar, INbChar, INcChar, INAlphaChar, INBetaChar, INGammaChar, INVolumeChar,DatabaseFile !InCollectionDay, InCollectionMonth, InCollectionYear
CHARACTER(LEN=250) :: BIGLINE
CHARACTER(LEN = 10) :: TolYoN
CHARACTER(LEN = 100), ALLOCATABLE ::  DBCollectionCode(:), DBCollectionDate(:), DBCollectionEntity(:), DBSystematicName(:)
CHARACTER(LEN = 1), ALLOCATABLE :: aTolFlag(:), bTolFlag(:), cTolFlag(:), AlphaTolFlag(:), BetaTolFlag(:), GammaTolFlag(:), VolTolFlag(:), TolFlagArray(:,:)
INTEGER :: NDatabaseEntries, DBE, SimilarityCount
REAL(KIND = 8) :: INa, INb, INc, INAlpha, INBeta, INGamma, INVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol, DefaultTol
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



!!!!!!! Write some gubbins

WRITE(6,*) '        ---------------------'
WRITE(6,*) '            CIFDB V2.0'
WRITE(6,*) '        By Jon G. C. Kragskow'
WRITE(6,*) '        ---------------------'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Read in database file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

OPEN(66, FILE = DatabaseFile, STATUS = 'OLD')

!Read Number of Database Entries
    READ(66,*) NDatabaseEntries
    READ(66,*) !READ BLANK
    READ(66,*) !READ BLANK
    READ(66,*) !READ BLANK
    ALLOCATE(DBCollectionCode(NDatabaseEntries), DBCollectionDate(NDatabaseEntries), DBCollectionEntity(NDatabaseEntries), DBSystematicName(NDatabaseEntries), DBa(NDatabaseEntries), DBb(NDatabaseEntries), DBc(NDatabaseEntries), DBAlpha(NDatabaseEntries), DBBeta(NDatabaseEntries), DBGamma(NDatabaseEntries), DBVolume(NDatabaseEntries), aTolFlag(NDatabaseEntries), bTolFlag(NDatabaseEntries), cTolFlag(NDatabaseEntries), AlphaTolFlag(NDatabaseEntries), BetaTolFlag(NDatabaseEntries), GammaTolFlag(NDatabaseEntries), VolTolFlag(NDatabaseEntries),TolFlagArray(NDatabaseEntries,7))

!Loop over number of entries and read information
    DO DBE = 1,NDatabaseEntries

        READ(66,'(A)') BIGLINE
        BIGLINE = Replace_Text(BIGLINE,',','~~')
        READ(BIGLINE,*) DBCollectionCode(DBE), DBCollectionDate(DBE),  DBCollectionEntity(DBE), DBSystematicName(DBE), DBa(DBE), DBb(DBE), DBc(DBE), DBAlpha(DBE), DBBeta(DBE), DBGamma(DBE), DBVolume(DBE)
        DBSystematicName(DBE) = ADJUSTL(TRIM(Replace_Text(DBSystematicName(DBE),'~~',',')))     
    END DO
CLOSE(66)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Set Tolerances for params !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DefaultTol = 0.05_8

!Allow user to set different tolerance or use global tolerance
WRITE(6,*)
WRITE(6,*) ' Tolerance Is +/-5% for all parameters'
WRITE(6,*) 'Would you like to change this? (y/n)'
WRITE(6,*)
READ(5,*) TolYoN
DO WHILE (TolYoN/='y' .AND. TolYoN/='n')
    WRITE(6,*)
    WRITE(6,*) 'Lets try that again Chief'
    WRITE(6,*) 'Would you like to change this? (y/n)'
    READ(5,*) TolYoN
END DO

IF (TolYoN == 'y') THEN
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

ELSE 
    aTol = DefaultTol
    bTol = DefaultTol
    cTol = DefaultTol
    alphaTol = DefaultTol
    betaTol = DefaultTol
    gammaTol = DefaultTol
    VolTol = DefaultTol
END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Find similar structures !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!cycle through database structures

SimilarityCount = 0

DO DBE = 1, NDatabaseEntries

    IF (RangeFall(INa, DBa(DBE),aTol) == 'y') THEN
        aTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        aTolFlag(DBE) = 'n'
    END IF

    IF (RangeFall(INb, DBb(DBE),bTol) == 'y') THEN
        bTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        bTolFlag(DBE) = 'n'
    END IF

    IF (RangeFall(INc, DBc(DBE),cTol) == 'y') THEN
        cTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        cTolFlag(DBE) = 'n'
    END IF

    IF (RangeFall(INalpha, DBalpha(DBE),alphaTol) == 'y') THEN
        AlphaTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        AlphaTolFlag(DBE) = 'n'
    END IF

    IF (RangeFall(INBeta, DBBeta(DBE),betaTol) == 'y') THEN
        BetaTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        BetaTolFlag(DBE) = 'n'
    END IF

    IF (RangeFall(INGamma, DBGamma(DBE),gammaTol) == 'y') THEN
        GammaTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        GammaTolFlag(DBE) = 'n'
    END IF

    IF (RangeFall(INVolume, DBVolume(DBE),VolTol) == 'y') THEN
        VolTolFlag(DBE) = 'y'
        SimilarityCount = SimilarityCount + 1
    ELSE
        VolTolFlag(DBE) = 'n'
    END IF

END DO

!Make Tolerance flag arrays (y and n)
TolFlagArray(:,1) = aTolFlag
TolFlagArray(:,2) = bTolFlag
TolFlagArray(:,3) = cTolFlag
TolFlagArray(:,4) = alphaTolFlag
TolFlagArray(:,5) = betaTolFlag
TolFlagArray(:,6) = gammaTolFlag
TolFlagArray(:,7) = volTolFlag


IF (SimilarityCount /= 0) THEN
    
    WRITE(6,*) 'Similarities:'
    WRITE(6,*) 
    
    DO DBE = 1, NDatabaseEntries
        IF (TolFlagArray(DBE,1) == 'y' .AND. TolFlagArray(DBE,2) == 'y' .AND. TolFlagArray(DBE,3) == 'y') THEN
            WRITE(6,'(A9, F7.3, A2, F7.3, A2, F7.3, A23, A7)') 'a, b, c (', Ina,', ', Inb,', ', Inc, ') are close to that of ', DBCollectionCode(DBE)
            WRITE(6,'(A9, F7.3, A2, F7.3, A2, F7.3, A1)') 'a, b, c (', DBa(DBE),', ', DBb(DBE),', ', DBc(DBE),')'
            WRITE(6,*)
        END IF

        IF (TolFlagArray(DBE,4) == 'y' .AND. TolFlagArray(DBE,5) == 'y' .AND. TolFlagArray(DBE,6) == 'y') THEN
            WRITE(6,'(A20, F7.3, A2, F7.3, A2, F7.3, A23, A7)') 'alpha, beta, gamma (', InAlpha,', ', InBeta,', ', INGamma, ') are close to that of ', DBCollectionCode(DBE)
            WRITE(6,'(A20, F7.3, A2, F7.3, A2, F7.3, A1)') 'alpha, beta, gamma (', DBAlpha(DBE),', ', DBBeta(DBE),', ', DBGamma(DBE),')'
            WRITE(6,*)
        END IF

        IF (TolFlagArray(DBE,7) == 'y') THEN
            WRITE(6,'(A8, F8.3, A22, A7)') 'Volume (', InVolume, ') is close to that of ', DBCollectionCode(DBE)
            WRITE(6,'(A8, F8.3, A1)') 'Volume (', DBVolume(DBE),')'
            WRITE(6,*)
        END IF


    END DO

ELSE
    WRITE(6,*) 
    WRITE(6,*)  'No Similarities'
    WRITE(6,*) 
END IF



CLOSE(66)



CONTAINS

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


FUNCTION Replace_Text (s,text,rep)  RESULT(outs)

!http://fortranwiki.org/fortran/show/String_Functions

CHARACTER(*)        :: s,text,rep
CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
INTEGER             :: i, nt, nr

outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
DO
   i = INDEX(outs,text(:nt)) 
   IF (i == 0) EXIT
   outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
END DO
END FUNCTION Replace_Text



END PROGRAM CIFDBCheck


