PROGRAM DBBuild
IMPLICIT NONE
CHARACTER(LEN=100) :: InputFile, InSystematicName, DatabaseFile, InCollectionDate, InCollectionEntity, InCollectionCode 
CHARACTER(LEN = 10) :: QuickFlag, InitialiseFlag
CHARACTER(LEN = 100), ALLOCATABLE ::  DBCollectionCode(:), DBCollectionEntity(:), DBSystematicName(:), DBCollectionDate(:)
INTEGER :: NDatabaseEntries
REAL(KIND = 8) :: INa, INb, INc, INAlpha, INBeta, INGamma, INVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol
REAL(KIND = 8), ALLOCATABLE :: DBa(:), DBb(:), DBc(:), DBAlpha(:), DBBeta(:),DBGamma(:), DBVolume(:)

call GET_COMMAND_ARGUMENT(1,InputFile)

IF(TRIM(ADJUSTL(InputFile)) == '-h' .or. TRIM(ADJUSTL(InputFile)) == '') THEN
               WRITE(6,*) 'DBBuild (NewCIFFile) (DatabaseFile) <QuickAdd (y or n)>'
                STOP
END IF


CALL GET_COMMAND_ARGUMENT(2,DatabaseFile)
CALL GET_COMMAND_ARGUMENT(3,QuickFlag)

!Quick flag to add/check entries with standard tolerances
IF (QuickFlag /= 'y') QuickFlag = 'n'

!Secret flag for InitialiseDB to skip printouts and checking
CALL GET_COMMAND_ARGUMENT(4,InitialiseFlag)

!!!!!!! Self Promotion
IF (InitialiseFlag /= 'y') CALL SelfPromo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Read input CIF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CALL ReadCIF(InputFile, InitialiseFlag, InCollectionCode, InSystematicName, InCollectionDate, InCollectionEntity, INa, Inb, Inc, InAlpha, InBeta, InGamma, InVolume)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Read in database file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Ask user to set tolerances !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CALL ReadNEntries(DatabaseFile, NDatabaseEntries)

IF (NDatabaseEntries > 0) THEN

    ALLOCATE(DBCollectionCode(NDatabaseEntries), DBCollectionDate(NDatabaseEntries), DBCollectionEntity(NDatabaseEntries), DBSystematicName(NDatabaseEntries), DBa(NDatabaseEntries), DBb(NDatabaseEntries), DBc(NDatabaseEntries), DBAlpha(NDatabaseEntries), DBBeta(NDatabaseEntries), DBGamma(NDatabaseEntries), DBVolume(NDatabaseEntries))

    CALL ReadDatabase(DatabaseFile, NDatabaseEntries, DBCollectionCode, DBCollectionDate, DBCollectionEntity, DBSystematicName, DBa, DBb, DBc, DBAlpha, DBBeta, DBGamma, DBVolume)

    CALL SetTolerances(QuickFlag, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol)

    CALL CheckAndWrite(QuickFlag, NDatabaseEntries, INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol, DBa, DBb, DBc, DBAlpha, DBbeta, DBGamma, DBVolume)

ELSE

CALL PrintFirstEntry(INa, INb, INc, InAlpha, InBeta, InGamma, InVolume)

END IF



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
WRITE(6,*)'                /     DBBuild V3.0     \  '
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

SUBROUTINE ReadCIF(InputFile, InitialiseFlag, InCollectionCode, InSystematicName, InCollectionDate, InCollectionEntity, INa, Inb, Inc, InAlpha, InBeta, InGamma, InVolume)
IMPLICIT NONE
INTEGER ::  DataFound, intdummy
CHARACTER(LEN = 10) :: InitialiseFlag
CHARACTER(LEN=100) :: InputFile, Line, InSystematicName, INaChar, INbChar, INcChar, INAlphaChar, INBetaChar, INGammaChar, INVolumeChar, InCollectionDate, InCollectionEntity, InCollectionCode, Cdummy
REAL(KIND = 8) :: INa, INb, INc, INAlpha, INBeta, INGamma, INVolume

!Open Error file if DBBuild called in initialise mode
IF (InitialiseFlag == 'y') THEN
    OPEN(55, FILE = 'DBERRORS.txt', STATUS = 'UNKNOWN',ACCESS = 'APPEND') 
ELSE
    InitialiseFlag='n'
END IF


!Set Default Values
InCollectionDate = 'UNKNOWN'
InSystematicName = 'UNKNOWN'
InCollectionEntity = 'UNKNOWN'


!Set tracking flag for whether data found or not
DataFound = 0

!Open input CIF and read out data fields
OPEN(33, FILE = InputFile, STATUS = 'OLD')

    DO WHILE(.true.)
        READ(33,'(A)',END=99) Line
        Line = ADJUSTL(Line)

        !Collection code
        IF(Line(1:5) == 'data_') THEN
            IF (LEN(ADJUSTL(TRIM(Line))) /= 5 .AND. DataFound /= 1) THEN 
                InCollectionCode = ADJUSTL(TRIM(Line(6:12)))
                DataFound = 1
            ELSE IF (LEN(ADJUSTL(TRIM(Line))) == 5) THEN
                WRITE(6,*) 'No collection code found in CIF     ', InputFile
                WRITE(6,*) 'DBBuild Aborts!'
                STOP
            ELSE IF (DataFound == 1) THEN
                WRITE(6,*) 'More than one dataset in CIF, only including first dataset'
                GOTO 99
            END IF
        END IF
    
        !Systematic Name
        IF(Line(1:25) == '_chemical_name_systematic') THEN
            IF (LEN(ADJUSTL(TRIM(Line))) /= 25) THEN 
                InSystematicName = Replace_Text(ADJUSTL(TRIM(Line(26:))),"'",'')
                InSystematicName = strcompress(InSystematicName,intdummy)

                IF (InSystematicName == '?') THEN
                    InSystematicName = 'UNKNOWN'
                    WRITE(6,*) 'Warning - No systematic name detected in CIF     ', InputFile
                    IF (InitialiseFlag == 'y') WRITE(55,*) 'No systematic name detected for file --  ',InputFile
                END IF
            ELSE 
                WRITE(6,*) 'Warning - No systematic name detected in CIF     ', InputFile
                IF (InitialiseFlag == 'y') WRITE(55,*) 'No systematic name detected for file --  ',InputFile
            END IF
        END IF

        !Collection Date
        IF(Line(1:30) == '_manchester_internal_coll_date') THEN
            IF (LEN(ADJUSTL(TRIM(Line))) /= 30) THEN 
                InCollectionDate = ADJUSTL(TRIM(Line(31:)))
                InCollectionDate = Replace_Text(InCollectionDate,'/','_')
            ELSE
                WRITE(6,*) 'Warning - No collection date detected in CIF     ', InputFile
                IF (InitialiseFlag == 'y') WRITE(55,*) 'No collection date detected for file --  ',InputFile
            END IF
        END IF

        !InCollection Client
        IF(Line(1:32) == '_manchester_internal_coll_client') THEN
            IF (LEN(ADJUSTL(TRIM(Line))) /= 32) THEN 
                InCollectionEntity = ADJUSTL(TRIM(Line(33:)))
                InCollectionEntity = Replace_Text(InCollectionEntity,'/','-')
            ELSE
                WRITE(6,*) 'Warning - No client detected in CIF     ', InputFile
                IF (InitialiseFlag == 'y') WRITE(55,*) 'No client detected for file          --  ',InputFile
            END IF
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
    
            !Read in and keep standard deviations for database??
            !Ask Fabs
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

IF (InitialiseFlag == 'y') CLOSE(55) !Close error file




END SUBROUTINE ReadCIF

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
CHARACTER(LEN = 100) :: DatabaseFile, DBCollectionCode(:), DBCollectionEntity(:), DBSystematicName(:), DBCollectionDate(:), YoN
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
    
            !Check for and report duplicate CIF then prompt user
            IF (InCollectionCode == DBCollectionCode(DBE)) THEN
                WRITE(6,*) '**********Duplicate CIF code found for    ', InputFile, '   ***********'
                WRITE(6,*) 'Would you like to continue? (y/n)'
                READ(5,*) YoN
                DO WHILE (YoN/='y' .AND. YoN/='n')
                    WRITE(6,*)
                    WRITE(6,*) 'Lets try that again Chief'
                    WRITE(6,*) 'Would you like to continue? (y/n)'
                    READ(5,*) YoN
                END DO
                IF (YoN == 'n') STOP
            END IF
    
    
        END DO
    
CLOSE(66)

END SUBROUTINE ReadDatabase

SUBROUTINE SetTolerances(QuickFlag, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol)
IMPLICIT NONE
CHARACTER(LEN = 5) :: YoN, QuickFlag
REAL(KIND = 8) :: DefaultTol, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol

DefaultTol = 5.0_8

aTol = DefaultTol
bTol = DefaultTol
cTol = DefaultTol
alphaTol = DefaultTol
betaTol = DefaultTol
gammaTol = DefaultTol
VolTol = DefaultTol


!Allow user to set different tolerance or use global tolerance
IF (QuickFlag /= 'y') THEN
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
END IF

END SUBROUTINE SetTolerances

SUBROUTINE CheckAndWrite(QuickFlag, NDatabaseEntries, INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, aTol, bTol, cTol, alphaTol, betaTol, gammaTol, VolTol, DBa, DBb, DBc, DBAlpha, DBbeta, DBGamma, DBVolume)
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

CALL PrintDB(QuickFlag, NDatabaseEntries, INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, DBa, DBb, DBc, DBAlpha, DBbeta, DBGamma, DBVolume)


END SUBROUTINE CheckAndWrite

SUBROUTINE PrintDB(QuickFlag, NDatabaseEntries, INa, INb, INc, InAlpha, InBeta, InGamma, InVolume, DBa, DBb, DBc, DBAlpha, DBbeta, DBGamma, DBVolume)
IMPLICIT NONE
INTEGER :: DBE, NDatabaseEntries
REAL(KIND = 8) :: INa, INb, INc, InAlpha, InBeta, InGamma, InVolume
REAL(KIND = 8) :: DBa(:), DBb(:), DBc(:), DBAlpha(:), DBBeta(:), DBGamma(:), DBVolume(:)
CHARACTER(LEN = 5) :: QuickFlag, YoN, RYoN

YoN = 'y'; RYoN = 'y'

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
END IF

    
IF (YoN == 'y' .AND. RYoN == 'y') THEN

    OPEN(66, FILE = DatabaseFile, STATUS = 'OLD')
    WRITE(66,'(I0)') NDatabaseEntries + 1
    WRITE(66,*)
    WRITE(66,*) '| CODE |     COLL. DATE    |   PERSON   |                                                 FORMULA                                            |    a    |    b    |    c    |  alpha  |   beta   |  gamma  |  Volume  |'
    WRITE(66,*)

    !Loop over existing entries
    DO DBE = 1, NDatabaseEntries
            WRITE(66,'(A7, A3, A10, A3, A15, A3, A100, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F9.3)') ADJUSTL(TRIM(DBCollectionCode(DBE))), '   ' , ADJUSTL(TRIM(DBCollectionDate(DBE))), '   ', ADJUSTL(TRIM(DBCollectionEntity(DBE))), '   ', ADJUSTL(TRIM(DBSystematicName(DBE))), '   ', DBa(DBE), '  ', DBb(DBE), '   ', DBc(DBE), '   ', DBalpha(DBE), '   ', DBbeta(DBE), '   ', DBGamma(DBE), '   ', DBVolume(DBE)
    END DO


    !Write new entry to database
    WRITE(66,'(A7, A3, A10, A3, A15, A3, A100, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F9.3)') ADJUSTL(TRIM(InCollectionCode)), '   ' , ADJUSTL(TRIM(InCollectionDate)), '   ', ADJUSTL(TRIM(InCollectionEntity)), '   ', ADJUSTL(TRIM(InSystematicName)), '   ', Ina, '   ', Inb, '   ', Inc, '   ', Inalpha, '   ', Inbeta, '   ', InGamma, '   ', InVolume

    CLOSE(66)

END IF

END SUBROUTINE PrintDB

SUBROUTINE PrintFirstEntry(INa, INb, INc, InAlpha, InBeta, InGamma, InVolume)
IMPLICIT NONE
REAL(KIND = 8) :: INa, INb, INc, InAlpha, InBeta, InGamma, InVolume

OPEN(66, FILE = DatabaseFile, STATUS = 'OLD')
WRITE(66,'(I0)') 1
WRITE(66,*)
WRITE(66,*) '| CODE |     COLL. DATE    |   PERSON   |                                                 FORMULA                                            |    a    |    b    |    c    |  alpha  |   beta   |  gamma  |  Volume  |'
WRITE(66,*)

!Write first entry to database
WRITE(66,'(A7, A3, A10, A3, A15, A3, A100, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F7.3, A3, F9.3)') ADJUSTL(TRIM(InCollectionCode)), '   ' , ADJUSTL(TRIM(InCollectionDate)), '   ', ADJUSTL(TRIM(InCollectionEntity)), '   ', ADJUSTL(TRIM(InSystematicName)), '   ', Ina, '   ', Inb, '   ', Inc, '   ', Inalpha, '   ', Inbeta, '   ', InGamma, '   ', InVolume

CLOSE(66)

END SUBROUTINE PrintFirstEntry


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


FUNCTION Cut_To_Char (s,cutchar)  RESULT(outs)
    !Truncates a string at a given character
    !e.g Cut_To_Char(Hello_World,_) = Hello

CHARACTER(*)        :: s,cutchar
CHARACTER(LEN(s)+100) :: outs
INTEGER             :: i

outs = s 
DO WHILE(.TRUE.)
   i = INDEX(outs,cutchar) 
   IF (i == 0) EXIT
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

FUNCTION strcompress( input_string, n ) RESULT ( output_string ) !Removes spaces from strings
    ! -- Arguments 
    CHARACTER( * ), INTENT( IN )  :: input_string 
    INTEGER,        INTENT( OUT ) :: n 
    ! -- Function result 
    CHARACTER( LEN( input_string ) ) :: output_string 
    ! -- Local parameters 
    INTEGER,        PARAMETER :: IACHAR_SPACE = 32, & 
                                 IACHAR_TAB   = 9 
    ! -- Local variables 
    INTEGER :: i 
    INTEGER :: iachar_character 
    ! -- Initialise output string 
    output_string = ' ' 
    ! -- Initialise output string "useful" length counter 
    n = 0 
    ! -- Loop over string elements 
    DO i = 1, LEN( input_string ) 
      ! -- Convert the current character to its position 
      ! -- in the ASCII collating sequence 
      iachar_character = IACHAR( input_string( i:i ) ) 
      ! -- If the character is NOT a space ' ' or a tab '->|' 
      ! -- copy it to the output string. 
      IF ( iachar_character /= IACHAR_SPACE .AND. & 
           iachar_character /= IACHAR_TAB         ) THEN 
        n = n + 1 
        output_string( n:n ) = input_string( i:i ) 
      END IF 
    END DO 

END FUNCTION strcompress


END PROGRAM DBBuild
