
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

      MODULE MIO_ASCII

      IMPLICIT NONE

      INTEGER, SAVE :: N_MIO_IN, N_MIO_OUT

      TYPE MIO_FILE_DATA_TYPE
          CHARACTER(16)              :: FILE_LOGICAL
          INTEGER                    :: NLAYS
          INTEGER                    :: BLEV
          INTEGER                    :: ELEV
          INTEGER                    :: NVARS
          CHARACTER(32), ALLOCATABLE :: VARNAMES(:)
          INTEGER,       ALLOCATABLE :: VARTYPES(:)
          CHARACTER(16), ALLOCATABLE :: VARUNITS(:)
          CHARACTER(256),ALLOCATABLE :: VARDESC (:)
          INTEGER,       ALLOCATABLE :: NDIMS(:)
          LOGICAL,       ALLOCATABLE :: L_TSTEP(:)
          LOGICAL,       ALLOCATABLE :: L_LAY(:)
          LOGICAL,       ALLOCATABLE :: L_COL(:)
          LOGICAL,       ALLOCATABLE :: L_ROW(:)
      END TYPE MIO_FILE_DATA_TYPE

      TYPE( MIO_FILE_DATA_TYPE ) :: MIO_FILE_DATA( 200 )

      CONTAINS

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE INIT_MIO_FILE_DATA

          N_MIO_IN  = 0
          N_MIO_OUT = 0

      END SUBROUTINE INIT_MIO_FILE_DATA

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE LOAD_MIO_FILE ( FILE_LOGICAL, NLAYS, BLEV, ELEV, VARNAMES, 
     &           VARTYPES, VARUNITS, VARDESC, NDIMS, 
     &           L_TSTEP, L_LAY, L_COL, L_ROW )


      IMPLICIT NONE

      CHARACTER(16), INTENT( IN )  :: FILE_LOGICAL
      INTEGER,       INTENT( IN )  :: NLAYS
      INTEGER,       INTENT( IN )  :: BLEV
      INTEGER,       INTENT( IN )  :: ELEV
      CHARACTER(32), INTENT( IN )  :: VARNAMES( : )
      INTEGER,       INTENT( IN )  :: VARTYPES( : )
      CHARACTER(16), INTENT( IN )  :: VARUNITS( : )
      CHARACTER(256),INTENT( IN )  :: VARDESC ( : )
      INTEGER,       INTENT( IN )  :: NDIMS( : )
      LOGICAL,       INTENT( IN )  :: L_TSTEP(:), L_LAY(:), 
     &                                L_COL(:), L_ROW(:)

      INTEGER NVARS

      NVARS = LEN( VARNAMES )
      N_MIO_OUT = N_MIO_OUT + 1

      MIO_FILE_DATA( N_MIO_OUT )%FILE_LOGICAL = FILE_LOGICAL
      MIO_FILE_DATA( N_MIO_OUT )%NLAYS = NLAYS
      MIO_FILE_DATA( N_MIO_OUT )%BLEV  = BLEV
      MIO_FILE_DATA( N_MIO_OUT )%ELEV  = ELEV
      MIO_FILE_DATA( N_MIO_OUT )%NVARS = NVARS

      ALLOCATE( MIO_FILE_DATA( N_MIO_OUT )%VARNAMES( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%VARTYPES( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%VARUNITS( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%VARDESC ( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%NDIMS   ( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%L_TSTEP ( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%L_LAY   ( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%L_COL   ( NVARS ) ,
     &          MIO_FILE_DATA( N_MIO_OUT )%L_ROE   ( NVARS ) )

      MIO_FILE_DATA( N_MIO_OUT )%VARNAMES = VARNAMES
      MIO_FILE_DATA( N_MIO_OUT )%VARTYPES = VARTYPES
      MIO_FILE_DATA( N_MIO_OUT )%VARUNITS = VARUNITS
      MIO_FILE_DATA( N_MIO_OUT )%VARDESC  = VARDESC
      MIO_FILE_DATA( N_MIO_OUT )%NDIMS    = NDIMS
      MIO_FILE_DATA( N_MIO_OUT )%L_TSTEP  = L_TSTEP
      MIO_FILE_DATA( N_MIO_OUT )%L_LAY    = L_LAY
      MIO_FILE_DATA( N_MIO_OUT )%L_COL    = L_COL
      MIO_FILE_DATA( N_MIO_OUT )%L_ROW    = L_ROW

      END SUBROUTINE LOAD_MIO_FILE

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE WRITE_MIO_ASCII ( FNAME )

C   This subroutine will create a mock-up of the MIO input file which
C   specifies the number of input and output files, their filenames, and
C   the variables on each file, along with metadata for each variable.
C   The subroutine is expected to be used to demonstrate that the
C   updates made to driver.F to initialize the CMAQ system are
C   successfully making available all of the information needed by MIO.
C   After these updates are confirmed, approved, and merged, it is
C   expected that this routine will either be removed, or it may be kept
C   as an optional diagnostic for MIO characterization. It should not be
C   used in default operations of CMAQ.
C-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE

      CHARACTER(256) :: FNAME
      INTEGER        :: IOUT
      CHARACTER(10)  :: VT
      CHARACTER(100) :: DIMSTRING

C-----------------------------------------------------------------------

      IOUT = JUNIT()
      OPEN( IOUT,  FILE = FNAME, STATUS = 'NEW' )

      WRITE ( IOUT, '(A2, I3, A3, I3)' ) 'in', N_MIO_IN, 
     &                                  'out', N_MIO_OUT

      ! Write Output File Metadata
      DO IFILE = 1,N_MIO_OUT
         WRITE ( IOUT ) 
         WRITE ( IOUT, '(A)' ) MIO_FILE_DATA( IFILE )%FILE_LOGICAL
         WRITE ( IOUT, '(I)' ) MIO_FILE_DATA( IFILE )%NVARS

         DO IVAR = 1,MIO_FILE_DATA( IFILE )%NVARS 
           ! Find string for vairbale type
           IF ( MIO_FILE_DATA( IFILE )%VARTYPE( IVAR ) .EQ. M3REAL ) VT = 'real'

           ! Make string for dimensions
           DIMSTRING = ''
           IF ( MIO_FILE_DATA( IFILE )%L_TSTEP(IVAR) ) DIMSTRING = TRIM(DIMSTRING) // ' TSTEP'
           IF ( MIO_FILE_DATA( IFILE )%L_LAY(IVAR) )   DIMSTRING = TRIM(DIMSTRING) // ' LAY'
           IF ( MIO_FILE_DATA( IFILE )%L_ROW(IVAR) )   DIMSTRING = TRIM(DIMSTRING) // ' ROW'
           IF ( MIO_FILE_DATA( IFILE )%L_COL(IVAR) )   DIMSTRING = TRIM(DIMSTRING) // ' COL'

           WRITE ( IOUT, '(A,1x,A,1x,A,A,1x,A1,A,A1,1x,A1,A,A1)' ) 
     &             MIO_FILE_DATA( IFILE )%VARNAMES( IVAR ), TRIM(VT),
     &             MIO_FILE_DATA( IFILE )%NDIMS( IVAR ),TRIM(DIMSTRING)
     &             '"',TRIM(MIO_FILE_DATA( IFILE )%VARUNITS( IVAR )),'"',
     &             '"',TRIM(ELMO_DESC( IFILE,IVAR )),'"'
         END DO
      END DO

      CLOSE( IOUT )

      END SUBROUTINE WRITE_MIO_ASCII

      END MODULE MIO_ASCII
