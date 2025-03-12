
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

C routines for aerosol formation and transformation processes

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AEROPROC( DT, COL, ROW, LAYER )

C-----------------------------------------------------------------------
C  SUBROUTINE AEROPROC advances the number, second moment, and mass
C   concentrations for each mode over the time interval DT.

C  KEY SUBROUTINES CALLED:
C     GETPAR, ORGAER5, GETCOAGS, INTERCOAG_GH, INTRACOAG_GH,
C     VOLINORG

C  KEY FUNCTIONS CALLED:  GETAF

C  REVISION HISTORY:
C     Coded in December 1999 by Dr. Francis S. Binkowski
C      Modified from older versions used in CMAQ.

C FSB 05/17/00  new version of RPMARES included

C FSB 05/30/00  Fixed minor bug in awater.f

C FSB Correction to extinction coefficient in getbext.

C FSB 07/21/00 corrected units on ORGRATES and ORGBRATE_IN from
C      ppm/min to ppm/sec in AEROPROC and AEROSTEP.

C FSB 11/30/00 following changes from 07/28/00
C     Fixed problem for new emissions file version
C     Combined emissions for M3
C     Used a fixed value of Dpmin for plotting
C     Added variables OMEGA_AT & OMEGA_AC for partitioning
C     Eliminated the restriction on relative humidity for nucleation.
C     Added a branch in EQL for very low relative humidity (<1%).

C FSB Following changes to RPMARES:
C     Number of iterations reduced from 150 to 50.
C     Iterations are used only if 0.5 < = RATIO.
C     In calculating the molality of the bisulfate ion, a
C     MAX(1.0e-10, MSO4 ) is used.

C FSB 08/08/01 Changes to NEWPART to correct mass rate and to AEROSTEP
C     to correct sulfate, include emissions, and trap problem in
C     accumulation mode number calculation.

C FSB 09/19/01 Version AE3, major changes
C     Organics are done with Dr. Benedikt Schell's approach. (ORGAER3)
C     Particle production uses Kulmala approach (NEWPART3)
C     Condensational factors for sulfate and organics are calculated
C      separately.
C     Emissions are assumed to be input in vertical diffusion.
C     Include files replaced by Fortran 90 Modules.
C     The modules also contain subroutines.

C FSB 10/24/01 Added the treatment of gaseous N2O5 -> aerosol HNO3

C FSB 10/25/01 Changed the mass transfer calculation for Aitken to
C     accumulation mode by coagulation as recommended by Dr. Benedikt
C     Schell.

C SJR 04/24/02 Replaced thermodynamic code RPMARES with ISORROPIA
C     --Shawn J. Roselle

C GLG 04/04/03 Modifications to allow for evaporation of semi-volatile
C     organics from aerosol phase.  --Gerald L. Gipson

C FSB 11/18/03 Corrections to sulfate condensation.  Previously,
C     SCONDRATE was undefined when SO4RATE=0 and SCONDRATE was
C     negative when SO4RATE < DMDT_so4.

C PVB 01/08/04 Several changes in preparation for simulation of 2001
C      calendar year --Dr. Prakash V. Bhave
C    -Added interface to new subroutine, GETCOAGS, for calculating
C      coagulation rates.  GETCOAGS is used instead of Gauss-Hermite
C      quadrature for computational efficiency, by setting
C      FASTCOAG_FLAG = .TRUE.  --PVB
C    -Removed SOA from the definition of "DRY" aerosol.  Aerosol surface
C      area is now transported without SOA.  See notes in GETPAR, AERO,
C      and AERO_DEPV subroutines.  --SJR
C    -Moved EQL3 call from AEROPROC to AEROSTEP, immediately following
C      the ORGAER3 call.  This is a side effect of transporting aerosol
C      surface area without SOA.  --SJR
C    -New subroutine, HCOND3, to calculate condensation rates for
C      2nd and 3rd moments.  Results are unchanged.  --FSB
C    -Revised method of calculating SOA.  Partition SOA to the modes
C      in proportion to the amounts of total organic mass (SOA plus
C      primary) in each mode.  Modal geometric standard deviations are
C      now preserved during SOA condensation and evaporation. --FSB
C    -Combined the former subroutines AEROPROC and AEROSTEP into this
C      subroutine; retained the name AEROPROC.  --PVB

C PVB 09/21/04 added in-line documentation with input from FSB.
C     Changed MWH2SO4 from 98.07354 to 98.0 g/mol for consistency with
C     the mechanism files.

C PVB 09/27/04 removed the IF(XM3.GT.0.0) mode-merging precondition
C     because it caused significant erroneous mode crossover.  Fix
C     suggested by Dr. Chris Nolte.

C PVB 01/19/05 Added SO4RATE to the EQL3 call vector.  This is necessary
C     to ensure that the gas and inorganic fine PM (i+j) concentrations
C     are in thermodynamic equilibrium at the end of each time step.

C PVB 05/02/05 Modified ERF statement function for negative arguments such
C     that erf(-x) = -erf(x).  Previous version had erf(-x) = erf(x).

C PVB 04/06/06 Added GAMMA_N2O5 to the AEROPROC and EQL3 call vectors,
C     so it can be written to the aerosol diagnostic file.

C SLN 09/07/07 Several changes in preparation for new SOA module
C    -Removed Aitken-mode SOA species (CBLK(VORGAI),CBLK(VORGBI)) and
C     all of the related variables (OMASS_I, OMASS_J, FRACI, FRACJ,
C     OLD_M2_I, OLD_M3_I, NEW_M2_I, NEW_M3_I)
C    -Replaced the other SOA species (CBLK(VORGAJ),CBLK(VORGBJ)) with
C     precursor-specific SOA species (e.g., CBLK(VTOL1J)).
C    -Replaced SOA_A and SOA_B with an array, SOA_ALL.  Replaced
C     OLDSOA_A and OLDSOA_B with an array, OLDSOA.  Updated ORGAER3
C     call vector accordingly.
C    -Removed dependence of mode-merging criteria on SOA condensation
C     because that logic was flawed.  Deleted all variables needed
C     for that calculation (CHEMRATE_ORG, ORGRATE, ORGBRATE).

C PVB 11/02/07 Moved heterogenous N2O5 chemistry from EQL3 to a new
C     subroutine, HETCHEM.

C PVB 11/29/07 Implementation of new SOA formation mechanisms
C    -Replaced call to ORGAER3 with a call to ORGAER5
C    -Moved CBLK updates of SV species, SOA species, M2, and M3 into
C     ORGAER5 instead of updating those values in AEROPROC

C JOY 04/08/08 White space, alignment, readability

C JTK 04/17/08 Implemented coarse chemistry updates
C     -replaced condensation calculations with call to VOLINORG
C     -added code for coarse surface area and variable coarse std. dev.
C     -modified modal dynamics equations because new particle formation
C      and growth are now calculated in VOLINORG

C JOY 12/07/08 Removed unused EQL3 code, add column and row arguments to
C     VOLINORG for diagnostic logging

C SH  12/17/09 Major restructuring of all aerosol codes
C     -use new Fortran modules (e.g., AERO_DATA, ORG_DEFN); shortened call
C      vector; replaced CBLK with aerospc structure; eliminated AERO_INFO

C SH  03/10/11 Renamed met_data to aeromet_data

C HS  03/10/11 Added call to POAAGE between ORGAER and HETCHEM

C SR  03/25/11 Replaced I/O API include files with UTILIO_DEFN

C BH  09/30/13 Removed Call to HETCHEM and GAMMA_N2O5 argument
C              because of merging of gas and heterogeneous 
C              chemistry

C JB  02/07/14 Added Jai Xing's mass balance fix when there is excessive
C              condensation or evaporation under cold conditions

C BH  07/21/14 Removed call to POAAGE because reaction represented in
C              chemical mechanism

C GS/KF 09/23/14 Updated nucleation scheme with Vehkamaki et al. (2002)

C HP/BM  4/16 Removed m3_wet_flag since wet/dry status is now
C             tracked by AERO_DATA variable. Moments are always wet in AEROPROC
C             Updated treatment of aerosol moments

C SLN 12/30/19 Update for ddm-3d implementation in version 5.3.1

C  REFERENCES:
C   1. Binkowski, F.S. and U. Shankar, The regional particulate matter
C      model 1. Model description and preliminary results, J. Geophys.
C      Res., Vol 100, No D12, 26101-26209, 1995.

C   2. Binkowski, F.S. and S.J. Roselle, Models-3 Community
C      Multiscale Air Quality (CMAQ) model aerosol component 1:
C      Model Description.  J. Geophys. Res., Vol 108, No D6, 4183
C      doi:10.1029/2001JD001409, 2003.

C   3. Bhave, P.V., S.J. Roselle, F.S. Binkowski, C.G. Nolte, S. Yu,
C      G.L. Gipson, and K.L. Schere, CMAQ aerosol module development:
C      recent enhancements and future plans, Paper No. 6.8, CMAS Annual
C      Conference, Chapel Hill, NC, 2004.
C-----------------------------------------------------------------------
      
      USE AERO_DATA
      USE PRECURSOR_DATA      ! gas phase aero precursor data
      USE ORG_DEFN
      USE AEROMET_DATA
      USE CONST
      USE UTILIO_DEFN
      USE AERO_BUDGET

#ifdef sens
      USE DDM3D_DEFN, ONLY : NP, NPMAX
      USE AERO_DDM3D, ONLY : s_aerospc_conc, s_precursor_conc,
     &                       s_so4rate
#endif


      IMPLICIT NONE

C *** arguments:
      REAL,    INTENT( IN ) :: DT          ! synchronization time step, sec
      INTEGER, INTENT( IN ) :: COL         ! Column of cell
      INTEGER, INTENT( IN ) :: ROW         ! Row of cell
      INTEGER, INTENT( IN ) :: LAYER       ! Layer of cell

C *** Parameters
      REAL, PARAMETER :: DIFFSULF = 9.36E-06  ! molecular diffusiviity for sulfuric acid
      REAL, PARAMETER :: SQRT2 = 1.4142135623731
      REAL, PARAMETER :: T0 = 288.15   ! [ K ]
      REAL, PARAMETER :: TWOTHIRDS   =  2.0 / 3.0
      REAL, PARAMETER :: ONE_OVER_TICE =  1.0 / 273.16

C *** local variables
      REAL         DIFFCORR   ! Correction to DIFFSULF & DIFFORG for pressure
      REAL         DV_SO4     ! molecular diffusivity of H2SO4 vapor after correction for ambient conditions
      REAL         SQRT_TEMP  ! square root of ambient temperature
      REAL         XLM        ! atmospheric mean free path [m]
      REAL         AMU        ! atmospheric dynamic viscosity [kg/m s]

      REAL( 8 ) :: CGR( N_MODE-1 ) ! Aitken & Accum. modes

      REAL( 8 ) :: LAMDA      ! mean free path [ m ]
      REAL( 8 ) :: KNC        ! KNC = TWO3 * DKBOLTZ *  AIRTEMP / AMU

C *** Free Molecular regime (depends upon modal density)
      REAL( 8 ) :: KFMAT      ! = SQRT( 3.0*DKBOLTZ * AIRTEMP / PDENSAT )
      REAL( 8 ) :: KFMAC      ! = SQRT( 3.0*DKBOLTZ * AIRTEMP / PDENSAC )
      REAL( 8 ) :: KFMATAC    ! = SQRT( 6.0*DKBOLTZ * AIRTEMP / ( PDENSAT + PDENSAC ) )

C *** Intermodal coagulation rates [ m**3/s ] ( 0th & 2nd moments )
      REAL( 8 ) :: BATAC( 2 ) ! Aitken to accumulation
      REAL( 8 ) :: BACAT( 2 ) ! accumulation from Aitken

C *** Intramodal coagulation rates [ m**3/s ] ( 0th & 2nd moments )
      REAL( 8 ) :: BATAT( 2 ) ! Aitken mode
      REAL( 8 ) :: BACAC( 2 ) ! accumulation mode

C *** Intermodal coagulation rate [ m**3/s ] ( 3rd moment )
      REAL( 8 ) :: C3IJ       ! Aitken to accumulation
      REAL( 8 ) :: C30ATAC    ! Aitken to accumulation
      REAL( 8 ) :: DG_D ( N_MODE )
      REAL( 8 ) :: SG_D ( N_MODE )
      REAL( 8 ) :: XXL_D( N_MODE )

! *** Variables for computing the budget
      REAL( 8 ) :: CBUDGET0_NUM ( N_MODE )
      REAL( 8 ) :: CBUDGET0_SRF ( N_MODE )
      REAL( 8 ) :: CBUDGET0_MASS( N_AEROSPC,N_MODE )

C *** variables for advancing concentrations one time step
      REAL( 8 ) :: A, B
      REAL( 8 ) :: Y0, Y
      REAL( 8 ) :: EXPDT
      REAL( 8 ) :: LOSS, PROD, POL
      REAL         TMASS
      REAL         FACTRANS ! special factor to compute mass transfer
      REAL         M30(3), M20(3), M00(3)      ! for initial conditions in time stepping

C *** Variables for mode merging
      REAL         GETAF
      REAL         AAA, XNUM, XM2, XM3,XXM2, XXM3
      REAL         FNUM, FM2, FM3, PHNUM, PHM2, PHM3

C *** local variables
      INTEGER      SPC        ! loop counter
      INTEGER      N          ! loop counter

      logical, save :: firstime = .true.
C-----------------------------------------------------------------------

      if ( firstime ) then
         firstime = .false.
#ifdef nomm
         write( logdev,* ) 'aero_subs: nomm'
#endif
      end if

C *** square root of the ambient temperature for later use
      SQRT_TEMP = SQRT( AIRTEMP )
 
C *** Calculate mean free path [ m ]:
C     6.6328E-8 is the sea level value given in Table I.2.8
C     on page 10 of U.S. Standard Atmosphere 1962

      XLM = 6.6328E-8 * STDATMPA * AIRTEMP  / ( T0 * AIRPRES )

C *** Calculate dynamic viscosity [ kg m**-1 s**-1 ]:
C     U.S. Standard Atmosphere 1962 page 14 expression
C     for dynamic viscosity is:
C     dynamic viscosity =  beta * T * sqrt(T) / ( T + S)
C     where beta = 1.458e-6 [ kg sec^-1 K**-0.5 ], s = 110.4 [ K ].
      AMU = 1.458E-6 * AIRTEMP * SQRT_TEMP / ( AIRTEMP + 110.4 )

C *** Set minimums for coarse mode
      MOMENT0_CONC( N_MODE ) = MAX( AEROMODE_MINNUM( N_MODE ),
     &                              MOMENT0_CONC( N_MODE ) )

C *** Secondary Organics
C     Update the secondary organic aerosol (SOA) mass concentrations
C     and the SVOC mass concentrations by equilibrium absorptive
C     partitioning between the particle and vapor phases.  Assume all
C     SOA resides in the accumulation mode.
C
C     Aerosol is wet when it enters orgaer
      CALL ORGAER( DT, LAYER )

C *** Secondary Inorganics
C     The VOLINORG subroutine includes the treatment of new particle
C     production and a fully dynamic treatment of inorganic gas-to-
C     particle mass transfer.

C *** Compute H2SO4 diffusivity, correct for temperature and pressure
      DIFFCORR = ( STDATMPA / AIRPRES ) * ( ONE_OVER_TICE * AIRTEMP ) ** 1.75
      DV_SO4 = DIFFSULF * DIFFCORR

C *** Update size parameters (distribution is wet from ORGAER)
      CALL GETPAR( FIXED_sg )

! *** Mass transfer of inorganic constituents      
! *** Process Budget is calculated inside this routine because NPF and
! *** condensation are calculated simultaneously
      CALL VOLINORG( DT, COL, ROW, LAYER, DV_SO4, CGR )

! *** Coagulation
!     Calculate coagulation coefficients using a method dictated by
!     the value of FASTCOAG_FLAG.  If TRUE, the computationally-
!     efficient GETCOAGS routine is used.  If FALSE, the more intensive
!     Gauss-Hermite numerical quadrature method is used.  See Section
!     2.1 of Bhave et al. (2004) for further discussion.
! *** Initialize Budget Variables
      CBUDGET0_NUM  = MOMENT0_CONC
      CBUDGET0_SRF  = MOMENT2_CONC * PI
      CBUDGET0_MASS = AEROSPC_CONC
 
! *** set atmospheric mean free path in double precision
      LAMDA    = XLM

! *** calculate term used in Equation A6 of Binkowski & Shankar (1995)
      KNC      = TWOTHIRDS * DKBOLTZ *  AIRTEMP / AMU

! *** calculate terms used in Equation A5 of Binkowski & Shankar (1995)
      KFMAT    = SQRT( 3.0 * DKBOLTZ * AIRTEMP / AEROMODE_DENS( 1 ) )
      KFMAC    = SQRT( 3.0 * DKBOLTZ * AIRTEMP / AEROMODE_DENS( 2 ) )
      KFMATAC  = SQRT( 6.0 * DKBOLTZ * AIRTEMP
     &         / ( AEROMODE_DENS( 1 ) + AEROMODE_DENS( 2 ) ) )

! *** transfer of number to accumulation mode from Aitken mode is zero
      BACAT( 1 ) = 0.0

      IF ( FASTCOAG_FLAG ) THEN ! Solve coagulation analytically

C *** set geometric mean diameters, geometric standard deviations, and
C     ln(GSD) in double precision
         DO N = 1, N_MODE
            DG_D( N ) = AEROMODE_DIAM( N )
            SG_D( N ) = EXP( AEROMODE_LNSG( N ) )
            XXL_D( N ) = AEROMODE_LNSG( N )
         END DO

C *** calculate intermodal and intramodal coagulation coefficients
C     for zeroth and second moments, and intermodal coagulation
C     coefficient for third moment
         CALL GETCOAGS( LAMDA, KFMATAC, KFMAT, KFMAC, KNC,
     &                  DG_D(1), DG_D(2), SG_D(1), SG_D(2),
     &                  XXL_D(1),XXL_D(2),
     &                  BATAT( 2 ), BATAT( 1 ), BACAC( 2 ), BACAC( 1 ),
     &                  BATAC( 2 ), BACAT( 2 ), BATAC( 1 ), C3IJ )

      ELSE                 ! Use Gauss-Hermite numerical quadrature

C *** calculate Aitken-mode intramodal coagulation coefficients
C     for zeroth and second moments
         CALL INTRACOAG_GH( LAMDA, KFMAT, KNC, AEROMODE_DIAM( 1 ),
     &                      AEROMODE_LNSG( 1 ), BATAT( 2 ), BATAT( 1 ) )

C *** calculate accumulation-mode intramodal coagulation coefficients
C     for zeroth and second moments
         CALL INTRACOAG_GH( LAMDA, KFMAC, KNC, AEROMODE_DIAM( 2 ),
     &                      AEROMODE_LNSG( 2 ), BACAC( 2 ), BACAC( 1 ) )

C *** calculate intermodal coagulation coefficients for zeroth, second,
C     and third moments
         CALL INTERCOAG_GH( LAMDA, KFMATAC, KNC,
     &                      AEROMODE_DIAM( 1 ), AEROMODE_DIAM( 2 ),
     &                      AEROMODE_LNSG( 1 ), AEROMODE_LNSG( 2 ),
     &                      BATAC( 2 ), BACAT( 2 ), BATAC( 1 ), C3IJ )

      END IF   ! FASTCOAG_FLAG

C *** calculate 3rd moment intermodal transfer rate by coagulation
      C30ATAC = C3IJ * MOMENT0_CONC( 1 ) * MOMENT0_CONC( 2 )

C *** TAKE ONE FORWARD TIME STEP - Solve Modal Dynamics Equations
C     This code implements Section 1.4 of Binkowski and Roselle (2003)
C     with two notable exceptions.  1) emissions are treated in
C     CMAQ`s vertical diffusion routine, so they do not appear in the
C     following equations. 2) new particle formation and condensational
C     growth are now treated in the VOLINORG subroutine, so they do not
C     appear in the following equations.
 
      ! Initial Moment Values
      M00 = MOMENT0_CONC
      M20 = MOMENT2_CONC
      M30 = MOMENT3_CONC

! *** Update Zeroth Moment (i.e. number concentration)
! *** Aitken mode: initial value of M0
      Y0 = MOMENT0_CONC( 1 )

      ! The rate of change for M0_Aitken is described in Equation 8a of
      ! Binkowski & Roselle (2003), with the c_i term equal to 0.
      A = BATAT( 1 )                      ! intramodal coagulation
      B = BATAC( 1 ) * moment0_conc( 2 )  ! intermodal coagulation

      EXPDT = EXP( - B * DT )
      IF ( EXPDT .LT. 1.0D0 ) THEN
         Y = B * Y0 * EXPDT / ( B + A * Y0 * ( 1.0D0 - EXPDT ) )
      ELSE
         Y = Y0                 ! solution in the limit that B approaches zero
      END IF

      ! Transfer new value of M0_Aitken to the moment array
      MOMENT0_CONC( 1 ) = MAX( AEROMODE_MINNUM( 1 ), REAL( Y ) )

      ! Accumulation mode: initial value of M0
      Y0 = MOMENT0_CONC( 2 )

      ! The rate of change for M0_accum is described in Equation 8b of
      ! Binkowski & Roselle (2003), except the coefficient C is zero
      ! because emissions are treated outside the CMAQ aerosol module.
      ! The equation reduces to the form: dY/dt = -A * Y**2 , where
      A = BACAC( 1 )                 ! intramodal coagulation

      ! Solve for M0_accum using Smoluchowski`s solution
      Y = Y0 / ( 1.0D0 + A * Y0 * DT )

      ! Transfer new value of M0_accum to the moment array
      MOMENT0_CONC( 2 ) = MAX( AEROMODE_MINNUM( 2 ), REAL( Y ) )

      ! ** Update Budget Variable for Zeroth Moment
      COAG_BUDGET( AERONUM_MAP( 1 ),1 ) = MOMENT0_CONC( 1 ) - CBUDGET0_NUM( 1 )
      COAG_BUDGET( AERONUM_MAP( 2 ),2 ) = MOMENT0_CONC( 2 ) - CBUDGET0_NUM( 2 )

! *** end of update for zeroth moment - note that the coarse mode number does
! *** not change because coarse-mode coagulation is neglected in the model
 

C *** UPDATE MASS CONCENTRATIONS (for each species)
C     The following procedure is described in Paragraphs 21-23
C     of Binkowski & Roselle (2003), except the Ei,n and Ej,n terms
C     are excluded here because emissions are treated outside the
C     CMAQ aerosol module.

C     Aitken mode mass concentration rates of change are of the form:
C        dc/dt = P - L*c    ! Equation 9a of Binkowski & Roselle (2003)
C     with solution
C        c(t0 + dt) = P/L + ( c(t0) - P/L ) * exp(-L*dt)

C     For all species, loss of Aitken mode mass is due to intermodal
C     coagulation.
C        LOSSn = PI/6 * RHOn * C30ATAC / MASSn
C        RHOn  = MASSn / (M3 * PI/6)
C     When above equations are combined, the PI/6 terms cancel yielding
C        LOSSn = C30ATAC / M3
C     where LOSSn is the loss rate of species n, RHOn is the mass of
C     species n per unit of particle volume, C30ATAC is the 3rd moment
C     loss rate due to intermodal coagulation, MASSn is the mass
C     concentration of species n, and M3 is the 3rd moment
C     concentration.

      LOSS = C30ATAC / MOMENT3_CONC( 1 )

      ! Set up extra variables to solve for Aitken mode mass concentrations
      FACTRANS = REAL( LOSS,4 ) * DT
      EXPDT = EXP( -FACTRANS )

      ! Transfer mass from Aitken to accumulation mode, resulting from 
      ! intermodal coagulation.

      DO SPC = 1, N_AEROSPC
         IF ( AERO_MISSING(SPC,1) ) CYCLE
 
         TMASS = AEROSPC_CONC( SPC,1 ) + AEROSPC_CONC( SPC,2 )
         IF( SPC .EQ. APHGJ_IDX  )THEN 
             ! assumes all production adsorb onto accumulation mode
             TMASS = TMASS + PHG_RATE * DT
         END IF

         AEROSPC_CONC( SPC,1 ) = MAX( AEROSPC( SPC )%MIN_CONC( 1 ),
     &                                REAL( AEROSPC_CONC( SPC,1 ) * EXPDT ) )
         AEROSPC_CONC( SPC,2 ) = MAX( AEROSPC( SPC )%MIN_CONC( 2 ),
     &                                TMASS - AEROSPC_CONC( SPC,1 ) )


         ! Update Budget Variable for Speciated Mass
         IF ( AEROSPC_MAP( SPC,1 ) .NE. 0 ) 
     &      COAG_BUDGET( AEROSPC_MAP( SPC,1 ),1 ) = 
     &           AEROSPC_CONC( SPC,1 ) - CBUDGET0_MASS( SPC,1 )
         IF ( AEROSPC_MAP( SPC,2 ) .NE. 0 ) 
     &      COAG_BUDGET( AEROSPC_MAP( SPC,2 ),1 ) = 
     &           AEROSPC_CONC( SPC,2 ) - CBUDGET0_MASS( SPC,2 )

      END DO

C *** end of update for species mass concentrations
 
! *** UPDATE SECOND MOMENT
      IF ( COAG_MOMENTS .EQ. 3 ) THEN
        ! For each lognormal mode, solve equations of form:
        !    dM2/dt = P2 - L2*M2   ! if L2 > 0
        ! with solution
        !    M2(t) = P2/L2 + ( M2(t0) - P2/L2 ) * exp( -L2*dt )
        ! or
        !    dM2/dt = P2           ! if L2 = 0
        ! with solution
        !    M2(t) = M2(t0) + P2*dt

        ! Loss of 2nd moment from Aitken mode is due to intermodal
        ! coagulation with accumulation mode and intramodal coagulation.
        ! Production term is removed, because new particle formation
        ! and condensational growth are accounted for in VOLINORG.
        LOSS = (
     &          ( BATAT( 2 ) * M00( 1 )
     &          + BATAC( 2 ) * M00( 2 ) ) * M00( 1 )
     &         ) / M20( 1 )

        ! Solve for M2_Aitken based on LOSS during this time step
        ! Note: LOSS is assumed to be non-negative.
        IF ( LOSS .GT. 0.0 ) THEN
           Y = M20( 1 ) * EXP( -LOSS * DT )
        ELSE
           Y = M20( 1 )
        END IF ! test on loss

        ! Transfer new value of M2_Aitken to the array
        MOMENT2_CONC( 1 ) = MAX( REAL( AEROMODE_MINM2( 1 ) ), REAL( Y ) )

        ! Production of 2nd moment in accumulation mode is due to
        ! intermodal coagulation Aitken mode
        PROD = BACAT( 2 ) * M00( 1 ) * M00( 2 )

        ! Loss of 2nd moment from accumulation mode is due only to
        ! intramodal coagulation
        LOSS = ( BACAC( 2 ) * M00( 2 ) * M00( 2 ) ) / M20( 2 )

        ! Solve for M2_accum based on PROD and LOSS during this time step
        ! Note: LOSS is assumed to be non-negative.
        IF ( LOSS .GT. 0.0 ) THEN
           POL = PROD / LOSS
           Y = POL + ( M20(2) - POL ) * EXP( -LOSS * DT )
        ELSE
           Y = M20(2) + PROD * DT
        END IF ! test on loss
     
        ! Transfer new value of M2_accum to moment array
        MOMENT2_CONC( 2 ) = MAX( REAL( AEROMODE_MINM2( 2 ) ), REAL( Y ) )

        ! Coarse mode: no change because coagulation of coarse particles
        ! is neglected in current model version.

      ELSE IF ( COAG_MOMENTS .EQ. 2 ) THEN
        ! Assume constant modal standard deviation and solve for new
        ! second moment as a function of the change in the third moment.
        MOMENT2_CONC( 1 ) = M20( 1 )*( MOMENT3_CONC( 1 ) / M30( 1 ) ) ** ( 2.0 / 3.0 )
        MOMENT2_CONC( 2 ) = M20( 2 )*( MOMENT3_CONC( 2 ) / M30( 2 ) ) ** ( 2.0 / 3.0 )

      END IF

      ! Update Budget Variable for Second Moment
      COAG_BUDGET( AEROSRF_MAP( 1 ),1 ) = MOMENT2_CONC( 1 ) * PI - CBUDGET0_SRF( 1 )
      COAG_BUDGET( AEROSRF_MAP( 2 ),2 ) = MOMENT2_CONC( 2 ) * PI - CBUDGET0_SRF( 2 )

C *** end of update for second moment
 

C *** Mode Merging
C     This code implements Section 1.5 of Binkowski and Roselle (2003).
C     If the Aitken mode mass is growing faster than accumulation mode
C     mass and the Aitken mode number concentration exceeds the
C     accumulation mode number concentration, then modes are merged by
C     renaming.

! *** Initialize Budget Variables
      CBUDGET0_NUM  = MOMENT0_CONC
      CBUDGET0_SRF  = MOMENT2_CONC * PI
      CBUDGET0_MASS = AEROSPC_CONC

#ifdef nomm
      if ( .false. ) then
#else
      IF ( CGR( 1 ) .GT. CGR( 2 ) .AND.
     &     MOMENT0_CONC( 1 ) .GT. MOMENT0_CONC( 2 ) ) THEN
#endif

C *** Before mode merging, update the third moments, geometric mean
C     diameters, geometric standard deviations, modal mass totals, and
C     particle densities, based on the new concentrations of M2, M0, and
C     speciated masses calculated above. This is still the wet
C     distribution.
         CALL GETPAR( FIXED_sg )

C *** Calculate AAA = ln( Dij / DGATK ) / ( SQRT2 * XXLSGAT ), where Dij
C     is the diameter at which the Aitken-mode and accumulation-mode
C     number distributions intersect (i.e., overlap).  AAA is equivalent
C     to the "Xnum" term described below Equation 10a by Binkowski and
C     Roselle (2003).
         AAA = GETAF( MOMENT0_CONC( 1 ), MOMENT0_CONC( 2 ),
     &                AEROMODE_DIAM( 1 ), AEROMODE_DIAM( 2 ), 
     &                AEROMODE_LNSG( 1 ), AEROMODE_LNSG( 2 ),
     &                SQRT2 ) 

C *** Ensure that Xnum is large enough so that no more than half of
C     the Aitken mode mass is merged into the accumulation mode during
C     any given time step.  This criterion is described in Paragraph 26
C     of Binkowski and Roselle (2003).
         XXM3 = 3.0 * AEROMODE_LNSG( 1 ) / SQRT2
         XNUM = MAX( AAA, XXM3 )

C *** Factors used in error function calls for M2 and M3 mode merging
         XXM2 = TWOTHIRDS * XXM3
         XM2  = XNUM - XXM2 ! set up for 2nd moment transfer
         XM3  = XNUM - XXM3 ! set up for 3rd moment and mass transfers

C *** Calculate the fractions of the number, 2nd, and 3rd moment
C     distributions with diameter greater than the intersection diameter
         FNUM  = 0.5 * ERFC( XNUM )            ! Eq 10a of B&R 2003
         FM2   = 0.5 * ERFC( XM2 )             ! Eq 10b of B&R 2003
         FM3   = 0.5 * ERFC( XM3 )             ! Eq 10b of B&R 2003

C *** Calculate the fractions of the number, 2nd, and 3rd moment
C     distributions with diameters less than the intersection diameter.
         PHNUM = 0.5 * ( 1.0 + ERF( XNUM ) )  ! Eq 10c of B&R 2003
         PHM2  = 0.5 * ( 1.0 + ERF( XM2 ) )   ! Eq 10d of B&R 2003
         PHM3  = 0.5 * ( 1.0 + ERF( XM3 ) )   ! Eq 10d of B&R 2003

C *** Update accumulation-mode moment concentrations using
C     Equations 11a - 11c of Binkowski and Roselle (2003).
         MOMENT0_CONC( 2 ) = MOMENT0_CONC( 2 ) + MOMENT0_CONC( 1 ) * FNUM
         MOMENT2_CONC( 2 ) = MOMENT2_CONC( 2 ) + MOMENT2_CONC( 1 ) * FM2
         MOMENT3_CONC( 2 ) = MOMENT3_CONC( 2 ) + MOMENT3_CONC( 1 ) * FM3

C *** Update Aitken-mode moment concentrations using
C     Equations 11d - 11f of Binkowski and Roselle (2003).
         MOMENT0_CONC( 1 ) = MOMENT0_CONC( 1 ) * PHNUM
         MOMENT2_CONC( 1 ) = MOMENT2_CONC( 1 ) * PHM2
         MOMENT3_CONC( 1 ) = MOMENT3_CONC( 1 ) * PHM3
 
C *** Rename masses of each species from Aitken mode to acumulation mode
C     using Equation 11b of Binkowski and Roselle (2003). Do this for
C     all species, even the aerosol water.
         DO SPC = 1, N_AEROSPC
            IF ( AERO_MISSING(SPC,1) ) CYCLE

            AEROSPC_CONC( SPC,2 ) = AEROSPC_CONC( SPC,2 ) + AEROSPC_CONC( SPC,1 ) * FM3
            AEROSPC_CONC( SPC,1 ) = AEROSPC_CONC( SPC,1 ) * PHM3
         END DO

      END IF ! end check on necessity for merging

C *** end of update for mode merging

C *** Update the third moments, geometric mean diameters, geometric
C     standard deviations, modal mass totals, and particle densities,
C     based on the final concentrations of M2, M0, and speciated masses
C     after mode merging is complete. This should be done for the wet
C     distribution.
      CALL GETPAR( FIXED_sg )

C *** Set minimum value for all concentrations in the CBLK array

      DO N = 1, N_MODE
         DO SPC = 1, N_AEROSPC
            AEROSPC_CONC( SPC,N ) = MAX( AEROSPC_CONC( SPC,N ),
     &                                   AEROSPC( SPC )%MIN_CONC( N ) )
#ifdef sens
            IF (AEROSPC_CONC( SPC,N ) .EQ. AEROSPC( SPC )%MIN_CONC( N ) )  THEN
               S_AEROSPC_CONC( SPC,N,: ) = 0.0
            ENDIF
#endif
         END DO
      END DO


! *** Propagate Mode Merging and Minimum value impacts to budget variables
      GROWTH_BUDGET( AERONUM_MAP(:) ) = MOMENT0_CONC(:) - CBUDGET0_NUM(:)
      GROWTH_BUDGET( AEROSRF_MAP(:) ) = MOMENT2_CONC(:) * PI - CBUDGET0_SRF(:)
      DO SPC = 1,N_AEROSPC
         WHERE( AEROSPC_MAP( SPC,: ) .NE. 0 )
     &          GROWTH_BUDGET( AEROSPC_MAP(SPC,:) )=
     &              AEROSPC_CONC(SPC,:) - CBUDGET0_MASS(SPC,:)
      END DO

      RETURN
      END SUBROUTINE AEROPROC


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      REAL FUNCTION GETAF( NI, NJ, DGNI, DGNJ, XLSGI, XLSGJ, SQRT2 )

C  Returns the value of "Xnum" in Equations 10a and 10c
C  of Binkowski and Roselle (2003), given the number concentrations,
C  median diameters, and natural logs of the geometric standard
C  deviations, in two lognormal modes.  The value returned by GETAF
C  is used subsequently in the mode merging calculations:
C       GETAF = ln( Dij / Dgi ) / ( SQRT2 * ln(Sgi) )
C  where Dij is the diameter of intersection,
C        Dgi is the median diameter of the smaller size mode, and
C        Sgi is the geometric standard deviation of smaller mode.
C  A quadratic equation is solved to obtain GETAF, following the
C  method of Press et al.
C 
C  References:
C   1. Binkowski, F.S. and S.J. Roselle, Models-3 Community
C      Multiscale Air Quality (CMAQ) model aerosol component 1:
C      Model Description.  J. Geophys. Res., Vol 108, No D6, 4183
C      doi:10.1029/2001JD001409, 2003.
C   2. Press, W.H., S.A. Teukolsky, W.T. Vetterling, and B.P.
C      Flannery, Numerical Recipes in Fortran 77 - 2nd Edition.
C      Cambridge University Press, 1992.

      IMPLICIT NONE

      REAL NI, NJ, DGNI, DGNJ, XLSGI, XLSGJ, SQRT2
      REAL AA, BB, CC, DISC, QQ, ALFA, L, YJI

C-----------------------------------------------------------------------

C *** Store intermediate values used for the quadratic solution
C     to reduce computational burden
      ALFA = XLSGI / XLSGJ
      YJI = LOG( DGNJ / DGNI ) / ( SQRT2 * XLSGI )
      L = LOG( ALFA * NJ / NI)

C *** Calculate quadratic equation coefficients & discriminant
      AA = 1.0 - ALFA * ALFA
      BB = 2.0 * YJI * ALFA * ALFA
      CC = L - YJI * YJI * ALFA * ALFA
      DISC = BB * BB - 4.0 * AA * CC

C *** If roots are imaginary, return a negative GETAF value so that no
C     mode merging takes place.
      IF ( DISC .LT. 0.0 ) THEN
         GETAF = - 5.0       ! error in intersection
         RETURN
      END IF

C *** Equation 5.6.4 of Press et al.
      QQ = -0.5 * ( BB + SIGN( 1.0, BB ) * SQRT( DISC ) )

C *** Return solution of the quadratic equation that corresponds to a
C     diameter of intersection lying between the median diameters of
C     the 2 modes.
      GETAF = CC / QQ       ! See Equation 5.6.5 of Press et al.

      RETURN
      END FUNCTION GETAF
 
 
