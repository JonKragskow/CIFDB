PROGRAM DBBuild
IMPLICIT NONE
CHARACTER(LEN=100) :: InputFile, Line, InSystematicName, Cdummy, INaChar, INbChar, INcChar, INAlphaChar, INBetaChar, INGammaChar, INVolumeChar,DatabaseFile,  InCollectionDate, InCollectionEntity,InCollectionCode !InCollectionDay, InCollectionMonth, InCollectionYear
CHARACTER(LEN = 10) :: YoN, RYoN, TolYoN, QuickFlag,Initialise,InitialiseFlag
CHARACTER(LEN = 100), ALLOCATABLE ::  DBCollectionCode(:), DBCollectionDate(:), DBCollectionEntity(:), DBSystematicName(:)
CHARACTER(LEN = 1), ALLOCATABLE :: aTolFlag(:), bTolFlag(:), cTolFlag(:), AlphaTolFlag(:), BetaTolFlag(:), GammaTolFlag(:), VolTolFlag(:), TolFlagArray(:,:)
INTEGER :: Nameflag,NDatabaseEntries, CollectionDateFlag, CollectionEntityFlag, DBE, SimilarityCount
REAL(KIND = 8) :: INa, INb, INc, INAlpha, INBeta, INGamma, INVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol, DefaultTol
REAL(KIND = 8), ALLOCATABLE :: DBa(:), DBb(:), DBc(:), DBAlpha(:), DBBeta(:),DBGamma(:), DBVolume(:)

call GET_COMMAND_ARGUMENT(1,InputFile)

IF(TRIM(ADJUSTL(InputFile)) == '-h' .or. TRIM(ADJUSTL(InputFile)) == '') THEN
               WRITE(6,*) 'DBBuild (NewCIFFile) (DatabaseFile) <QuickAdd (y or n)>'
                STOP
END IF


call GET_COMMAND_ARGUMENT(2,DatabaseFile)
call GET_COMMAND_ARGUMENT(3,QuickFlag)

IF (QuickFlag=='y') THEN
    QuickFlag='y'
ELSE
    QuickFlag='n'
END IF

!Secret flag for InitialiseDB to use to create 1st entry
call GET_COMMAND_ARGUMENT(4,InitialiseFlag)

IF (InitialiseFlag=='y') THEN
    Initialise='y'
ELSE
    Initialise='n'
END IF


!!!!!!! Write some gubbins
WRITE(6,*) '        ---------------------'
WRITE(6,*) '            DBBuild V2.0'
WRITE(6,*) '        By Jon G. C. Kragskow'
WRITE(6,*) '        ---------------------'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Read input CIF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Set flags for optional fields
NameFlag = 0
CollectionDateFlag = 0
CollectionEntityFlag = 0


OPEN(33, FILE = InputFile, STATUS = 'OLD')

    DO WHILE(.true.)
                READ(33,'(A)',END=99) Line
                Line = ADJUSTL(Line)
                !Collection code
                IF(Line(1:5) == 'data_') THEN
                    IF (LEN(ADJUSTL(TRIM(Line))) /= 5) THEN 
                        InCollectionCode = ADJUSTL(TRIM(Line(6:12)))
                    ELSE
                        WRITE(6,*) 'No collection code found in CIF!'
                        WRITE(6,*) 'DBBuild Aborts!'
                        STOP
                    END IF
                END IF
    
                !Systematic Name
                IF(Line(1:25) == '_chemical_name_systematic') THEN
                    IF (LEN(ADJUSTL(TRIM(Line))) /= 25) THEN 
                        InSystematicName = Replace_Text(ADJUSTL(TRIM(Line(26:))),"'",'')
                        NameFlag = 1
                    ELSE
                        WRITE(6,*) 'Warning - No systematic name detected in CIF file'
                    END IF
                END IF
                !Collection Date
                !IF(Line(1:30) == '_manchester_internal_coll_date') THEN
                !    CollectionDateFlag = 1
                !    READ(Line, *) Cdummy, InCollectionDay, InCollectionMonth, InCollectionYear
                !    WRITE(6,*) InCollectionMonth
                !    InCollectionDate = ADJUSTL(TRIM(InCollectionDate))//'_'//ADJUSTL(TRIM(InCollectionMonth))//'_'//ADJUSTL(TRIM(InCollectionYear))
                !END IF
                IF(Line(1:30) == '_manchester_internal_coll_user') THEN
                    CollectionEntityFlag = 1
                    READ(Line, *) Cdummy, InCollectionEntity
                END IF
                
                !Cell information
                IF(Line(1:25) == '_cell_length_a') THEN
                    READ(Line,*) Cdummy, INaChar
                    READ(33,*) Cdummy,INbChar
                    READ(33,*) Cdummy,INcChar
                    READ(33,*) Cdummy,INAlphaChar
                    READ(33,*) Cdummy,INBetaChar
                    READ(33,*) Cdummy,INGammaChar
                    READ(33,*) Cdummy,INVolumeChar
    
                    !read in and keep standard deviations for database??
                    !Ask Fabs but probably!
                    !Character cut up to (
                    !Replace ( with nothing and then read number
    
                    INaChar = Cut_To_Char(INaChar,'(')
                    INbChar = Cut_To_Char(INbChar,'(')
                    INcChar = Cut_To_Char(INcChar,'(')
                    INAlphaChar = Cut_To_Char(INAlphaChar,'(')
                    INBetaChar = Cut_To_Char(INBetaChar,'(')
                    INGammaChar = Cut_To_Char(INGammaChar,'(')
                    INVolumeChar = Cut_To_Char(INVolumeChar,'(')
                    
                    READ(INaChar,*) INa
                    READ(INbChar,*) INb
                    READ(INcChar,*) INc
                    READ(INAlphaChar,*) INAlpha
                    READ(INBetaChar,*) INBeta
                    READ(INGammaChar,*) INGamma
                    READ(INVolumeChar,*) INVolume
    
    
                END IF
    END DO
    99 CONTINUE

CLOSE(33)


IF (CollectionDateFlag == 0) InCollectionDate = 'UNKNOWN'
IF (Nameflag == 0) InSystematicName = 'UNKNOWN'
IF (CollectionEntityFlag == 0) InCollectionEntity = 'UNKNOWN'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Read in database file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF (Initialise=='n') THEN
    OPEN(66, FILE = DatabaseFile, STATUS = 'OLD')
    
    !Read Number of Database Entries
        
        READ(66,*) NDatabaseEntries
        READ(66,*) !READ BLANK
        READ(66,*) !READ BLANK
        READ(66,*) !READ BLANK
        ALLOCATE(DBCollectionCode(NDatabaseEntries), DBCollectionDate(NDatabaseEntries), DBCollectionEntity(NDatabaseEntries), DBSystematicName(NDatabaseEntries), DBa(NDatabaseEntries), DBb(NDatabaseEntries), DBc(NDatabaseEntries), DBAlpha(NDatabaseEntries), DBBeta(NDatabaseEntries), DBGamma(NDatabaseEntries), DBVolume(NDatabaseEntries), aTolFlag(NDatabaseEntries), bTolFlag(NDatabaseEntries), cTolFlag(NDatabaseEntries), AlphaTolFlag(NDatabaseEntries), BetaTolFlag(NDatabaseEntries), GammaTolFlag(NDatabaseEntries), VolTolFlag(NDatabaseEntries),TolFlagArray(NDatabaseEntries,7))
    
    !Loop over number of entries and read information
        DO DBE = 1,NDatabaseEntries
            READ(66,*) DBCollectionCode(DBE), DBCollectionDate(DBE), DBCollectionEntity(DBE), DBSystematicName(DBE), DBa(DBE), DBb(DBE), DBc(DBE), DBAlpha(DBE), DBBeta(DBE), DBGamma(DBE), DBVolume(DBE)
        END DO
    
    CLOSE(66)


    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Set Tolerances for params !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    
    IF (quickflag /= 'y') THEN
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
    END IF
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Find similar structures !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Cycle through entries and check each parameter against the database 
    
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
    
    
    IF ( quickflag /= 'y') THEN
        IF (SimilarityCount > 0) THEN

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
            WRITE(6,*) 'No Similarities'
            WRITE(6,*) 
        END IF
    END IF
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Write out final database file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELSE

    NDatabaseEntries = 0

END IF

IF ( quickflag /= 'y') THEN
    WRITE(6,*) 'Would you like to add this entry to the Database? (y/n)'
    READ(5,*) YoN
    DO WHILE(YoN /= 'y' .AND. YoN /= 'n')
        WRITE(6,*)
        WRITE(6,*) 'Lets try that again Chief'
        WRITE(6,*) 'Would you like to add this entry to the Database? (y/n)'
        READ(5,*) YoN
    END DO
    IF (YoN == 'y') THEN
        WRITE(6,*)
        WRITE(6,*) 'Are you sure you want to add this entry (y/n)'
        READ(5,*) RYoN
        DO WHILE (RYoN /= 'y' .AND. RYoN /= 'n')
            WRITE(6,*)
            WRITE(6,*) 'Lets try that again Chief'
            WRITE(6,*) 'Are you sure you want this entry (y/n)'
            READ(5,*) RYoN
        END DO
    END IF

ELSE

    YoN = 'y'; RYoN = 'y'

END IF

IF (YoN == 'y' .AND. RYoN == 'y') THEN

    !CALL SYSTEM('rm '//TRIM(ADJUSTL(DatabaseFile))) !Not neccesary to delete file as it is rewritten anyways

    OPEN(66, FILE = DatabaseFile, STATUS = 'UNKNOWN')
    WRITE(66,'(I0)') NDatabaseEntries + 1
    WRITE(66,*)
    WRITE(66,*) '  CODE |  COLLDATE  |     PERSON     |              FORMULA          |    a    |    b    |    c    |  alpha  |   beta   |  gamma  |  Volume  |'
    WRITE(66,*)

    !Loop over existing entries
    IF (Initialise == 'n') THEN
        DO DBE = 1, NDatabaseEntries
            WRITE(66,'(A7, A3, A8, A3, A4, A3, A40, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F9.3)') ADJUSTL(TRIM(DBCollectionCode(DBE))), '   ', ADJUSTL(TRIM(DBCollectionDate(DBE))), '   ', ADJUSTL(TRIM(DBCollectionEntity(DBE))), '   ', ADJUSTL(TRIM(DBSystematicName(DBE))), '   ', DBa(DBE), '  ', DBb(DBE), '   ', DBc(DBE), '   ', DBalpha(DBE), '   ', DBbeta(DBE), '   ', DBGamma(DBE), '   ', DBVolume(DBE)
        END DO
    END IF

    !Write extra entry to database
    WRITE(66,'(A7, A3, A8, A3, A4, A3, A40, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F9.3)') ADJUSTL(TRIM(InCollectionCode)), '   ', ADJUSTL(TRIM(InCollectionDate)), '   ', ADJUSTL(TRIM(InCollectionEntity)), '   ', ADJUSTL(TRIM(InSystematicName)), '   ', Ina, '   ', Inb, '   ', Inc, '   ', Inalpha, '   ', Inbeta, '   ', InGamma, '   ', InVolume
    WRITE(6,'(A7, A3, A8, A3, A4, A3, A40, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F9.3)') ADJUSTL(TRIM(InCollectionCode)), '   ', ADJUSTL(TRIM(InCollectionDate)), '   ', ADJUSTL(TRIM(InCollectionEntity)), '   ', ADJUSTL(TRIM(InSystematicName)), '   ', Ina, '   ', Inb, '   ', Inc, '   ', Inalpha, '   ', Inbeta, '   ', InGamma, '   ', InVolume

    CLOSE(66)

END IF



CLOSE(66)



CONTAINS

FUNCTION Replace_Text (s,text,rep)  RESULT(outs)

!http://fortranwiki.org/fortran/show/String_Functions

CHARACTER(*)        :: s,text,rep
CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
INTEGER             :: i, nt, nr

outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
DO
   i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
   outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
END DO
END FUNCTION Replace_Text


FUNCTION Cut_To_Char (s,cutchar)  RESULT(outs)
    !http://fortranwiki.org/fortran/show/String_Functions


CHARACTER(*)        :: s,cutchar
CHARACTER(LEN(s)+100) :: outs
INTEGER             :: i

outs = s 
DO
   i = INDEX(outs,cutchar) ; IF (i == 0) EXIT
   outs = outs(1:i-1)
END DO
END FUNCTION Cut_To_Char

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

END PROGRAM DBBuild
