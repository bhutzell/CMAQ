#!/bin/csh -f

# ===================== CBOXv60 Script ========================= 
# Usage: run.cctm >&! cctm_Bench_2018_12SE1.log &                                
#
# To report problems or request help with this script/program:     
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org  (CMAS Website)
# ===================================================================  

#> Simple Linux Utility for Resource Management System 
#> (SLURM) - The following specifications are recommended 
#> for executing the runscript on the cluster at the 
#> National Computing Center used primarily by EPA.
#SBATCH -t 1:00:00
# BATCH -n 1
#SBATCH -J CMAQ_Bench
# BATCH -p ord
# BATCH -p largemem
#SBATCH -p singlepe
#SBATCH -A mod3dev
#SBATCH -o /work/MOD3DEV/hwo/cmaq_testbed/RESEARCH_May-27-2025_parallel/CCTM/scripts/bench_%j.txt

#> The following commands output information from the SLURM
#> scheduler to the log files for traceability.
   if ( $?SLURM_JOB_ID ) then
      echo Job ID is $SLURM_JOB_ID
      echo "Running on nodes `printenv SLURM_JOB_NODELIST`"
      echo Host is $SLURM_SUBMIT_HOST
      #> Switch to the working directory. By default,
      #>   SLURM launches processes from your home directory.
      echo Working directory is $SLURM_SUBMIT_DIR
      cd $SLURM_SUBMIT_DIR
   endif

#> Configure the system environment and set up the module 
#> capability
   limit stacksize unlimited
#

# ===================================================================
#> Runtime Environment Options
# ===================================================================

echo 'Start Model Run At ' `date`

#> Toggle Diagnostic Mode which will print verbose information to 
#> standard output
 setenv CTM_DIAG_LVL 0

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 if ( ! $?compiler ) then
   setenv compiler intel
#  setenv compiler pgi
#  setenv compiler gcc
 endif
 if ( ! $?compilerVrsn ) then
   setenv compilerVrsn Empty
 endif

#> Source the config.cmaq file to set the build environment
 cd ../..
 source ./config_cmaq.csh $compiler $compilerVrsn
 cd CCTM/scripts

#> Set General Parameters for Configuring the Simulation
 set PROC      = serial            #> serial or mpi
 setenv MECH     cracmm3           #> Mechanism ID
#setenv MECH     cb6r5_ae7_aq      #> Mechanism ID
 set VRSN      = v60_${MECH}       #> Code Version
 set APPL      = Test              #> Application Name (e.g. Gridname)

#> Check that mechanism is cb6 or cracmm
 if ! ( ${MECH} =~ *cb6* || ${MECH} =~ *cracmm* ) then
     echo "ERROR: Mechanism must be a cb6 or cracmm variant but '${MECH}' was selected"
     exit 1
 endif

#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 setenv RUNID  ${VRSN}_${compilerString}_${APPL}

#> Set the build directory (this is where the CMAQ executable
#> is located by default).
 set BLD       = ${CMAQ_HOME}/CCTM/scripts/BLD_CBOX_${VRSN}_${compilerString}
 set EXEC      = CBOX_${VRSN}.exe  

#> Output Each line of Runscript to Log File
 if ( $CTM_DIAG_LVL != 0 ) set echo 

#> Set Working, Input, and Output Directories
 setenv WORKDIR ${CMAQ_HOME}/CCTM/scripts          #> Working Directory. Where the runscript is.
 setenv OUTDIR  ${CMAQ_DATA}/output_CBOX_${RUNID}  #> Output Directory
 setenv INPDIR  Empty  #Input Directory
 setenv LOGDIR  ${OUTDIR}/LOGS     #> Log Directory Location
 setenv NMLpath ${BLD}             #> Location of Namelists. Common places are: 
                                   #>   ${WORKDIR} | ${CCTM_SRC}/MECHS/${MECH} | ${BLD}

 echo ""
 echo "Working Directory is $WORKDIR"
 echo "Build Directory is $BLD"
 echo "Output Directory is $OUTDIR"
 echo "Log Directory is $LOGDIR"
 echo "Executable Name is $EXEC"

# =====================================================================
#> CCTM Configuration Options
# =====================================================================

#> Set Start and End Days for looping
 setenv NEW_START TRUE             #> Set to FALSE for model restart
 set START_DATE = "2018-07-01"     #> beginning date (July 1, 2016)
 set END_DATE   = "2018-07-01"     #> ending date    (July 1, 2016)

#> Set Timestepping Parameters
set STTIME     = 000000            #> beginning GMT time (HHMMSS)
set NSTEPS     = 240000            #> time duration (HHMMSS) for this run
set TSTEP      = 003000            #> output time step interval (HHMMSS)

#> Horizontal domain decomposition
if ( $PROC == serial ) then
   setenv NPCOL_NPROW "1 1"; set NPROCS   = 1 # single processor setting
else
   @ NPCOL  =  1; @ NPROW =  1
   @ NPROCS = $NPCOL * $NPROW
   setenv NPCOL_NPROW "$NPCOL $NPROW"; 
endif

#> Define Execution ID: e.g. [CMAQ-Version-Info]_[User]_[Date]_[Time]
if ( ! -e ${BLD}/cbox_${VRSN}.cfg ) then
   set SHAID = ""
else
   set SHAID = `grep "sha_ID" ${BLD}/cbox_${VRSN}.cfg | cut -c 13-22`
   if ( $SHAID == not_a_repo ) then
     set SHAID = ""
   else
     set SHAID = "_sha="$SHAID
   endif
endif
setenv EXECUTION_ID "CMAQ_CBOX${VRSN}${SHAID}_`id -u -n`_`date -u +%Y%m%d_%H%M%S_%N`"    #> Inform IO/API of the Execution ID
echo ""
echo "---CMAQ EXECUTION ID: $EXECUTION_ID ---"

#> Keep or Delete Existing Output Files
set CLOBBER_DATA = TRUE 

#> Logfile Options
#> Master Log File Name; uncomment to write standard output to a log, otherwise write to screen
#setenv LOGFILE $CMAQ_HOME/$RUNID.log  
if (! -e $LOGDIR ) then
  mkdir -p $LOGDIR
endif
setenv PRINT_PROC_TIME Y           #> Print timing for all science subprocesses to Logfile
                                   #>   [ default: TRUE or Y ]
setenv STDOUT T                    #> Override I/O-API trying to write information to both the processor 
                                   #>   logs and STDOUT [ options: T | F ]

setenv GRID_NAME 2018_12NE3COL         #> check GRIDDESC file for GRID_NAME options
#setenv GRID_NAME 2018_12NE3         #> check GRIDDESC file for GRID_NAME options
setenv GRIDDESC $WORKDIR/GRIDDESC_12NE3    #> grid description file

#> Retrieve the number of columns, rows, and layers in this simulation
set NZ = 35
set NX = `grep -A 1 ${GRID_NAME} ${GRIDDESC} | tail -1 | sed 's/  */ /g' | cut -d' ' -f6`
set NY = `grep -A 1 ${GRID_NAME} ${GRIDDESC} | tail -1 | sed 's/  */ /g' | cut -d' ' -f7`
set NCELLS = `echo "${NX} * ${NY} * ${NZ}" | bc -l`

#> Output Species and Layer Options
   #> CONC file species; comment or set to "ALL" to write all species to CONC
   setenv CONC_SPCS "ALL"
!  setenv CONC_SPCS "O3"
   setenv CONC_BLEV_ELEV " 1 1" #> CONC file layer range; comment to write all layers to CONC

   #> ACONC file species; comment or set to "ALL" to write all species to ACONC
   setenv AVG_CONC_SPCS "ALL" 
   setenv ACONC_BLEV_ELEV " 1 1" #> ACONC file layer range; comment to write all layers to ACONC
   setenv AVG_FILE_ENDTIME N     #> override default beginning ACONC timestamp [ default: N ]

#> Synchronization Time Step and Tolerance Options
setenv CTM_MAXSYNC 300       #> max sync time step (sec) [ default: 720 ]
setenv CTM_MINSYNC  60       #> min sync time step (sec) [ default: 60 ]
setenv SIGMA_SYNC_TOP 0.97    #> top sigma level thru which sync step determined [ default: 0.7 ] 
#setenv ADV_HDIV_LIM 0.95    #> maximum horiz. div. limit for adv step adjust [ default: 0.9 ]
setenv CTM_ADV_CFL 0.95      #> max CFL [ default: 0.75]
#setenv RB_ATOL 1.0E-09      #> global ROS3 solver absolute tolerance [ default: 1.0E-07 ] 

#> Science Options
setenv CTM_OCEAN_CHEM Y      #> Flag for ocean halogen chemistry, sea spray aerosol emissions,
                             #> and enhanced ozone deposition over ocean waters  [ default: Y ]
setenv CTM_WB_DUST N         #> use inline windblown dust emissions (only for use with PX) [ default: N ]
setenv CTM_LNO_ONLINE N      #> turn on lightning NOx [ default: N ]
                             #> alternatively LNOx emissions can also be read in as external emissions inputs,
                             #> in this case, please setenv this variable to N to avoid double counting
setenv KZMIN Y               #> use Min Kz option in edyintb [ default: Y ], 
                             #>    otherwise revert to Kz0UT
setenv PX_VERSION Y          #> WRF PX LSM
setenv CLM_VERSION N         #> WRF CLM LSM
setenv NOAH_VERSION N        #> WRF NOAH LSM
setenv CTM_ABFLUX N          #> ammonia bi-directional flux for in-line deposition 
                             #>    velocities [ default: N ]
setenv CTM_BIDI_FERT_NH3 N   #> subtract fertilizer NH3 from emissions because it will be handled
                             #>    by the BiDi calculation [ default: Y ]
setenv CTM_HGBIDI N          #> mercury bi-directional flux for in-line deposition 
                             #>    velocities [ default: N ]
setenv CTM_SFC_HONO N        #> surface HONO interaction [ default: Y ]
                             #> please see user guide (6.10.4 Nitrous Acid (HONO)) 
                             #> for dependency on percent urban fraction dataset
setenv CTM_GRAV_SETL Y       #> vdiff aerosol gravitational sedimentation [ default: Y ]
setenv CTM_PVO3 N            #> consider potential vorticity module for O3 transport from the stratosphere 
                             #>    [default: N]

setenv CTM_BIOGEMIS_BE N     #> calculate in-line biogenic emissions with BEIS [ default: N ]
setenv CTM_BIOGEMIS_MG N     #> turns on MEGAN biogenic emission [ default: N ]
setenv BDSNP_MEGAN N         #> turns on BDSNP soil NO emissions [ default: N ]
setenv USE_SEGA_N N          #> turns on EPA soil NO and HONO emissions [ default: N ]
setenv USE_SEGA_N_EF N       #> Use BEIS input emission factor file for soil NO and HONO [ default: N ]

setenv AEROSOL_OPTICS 3      #> sets method for determining aerosol optics affecting photolysis
                             #> frequencies ( 3 is the default value )
                             #>  VALUES 1 thru 3 determined Uniformly Volume Mixed spherical
                             #>      (1-Tabular Mie; 2-Mie Calculation; 3-Case Approx to Mie Theory)
                             #>  VALUES 4 thru 6 attempts to use core-shell mixing model when the
                             #>      aerosol mode has signficant black carbon core otherwise use Volume Mixed
                             #>      model where optics determined by
                             #>      (4-Tabular Mie; 5-Mie Calculation; 6-Case Approx to Mie Theory)

#> Surface Tiled Aerosol and Gaseous Exchange Option
setenv CTM_USE_STAGE Y       #> Use the STAGE deposition option [ default: N ]
setenv CTM_MOSAIC N          #> Output landuse specific deposition velocities [ default: N ]
setenv CTM_STAGE_P22 N       #> Pleim et al. 2022 Aerosol deposition model [default: N]
setenv CTM_STAGE_E20 Y       #> Emerson et al. 2020 Aerosol deposition model [default: Y; active only if CTM_USE_STAGE = Y]
setenv CTM_STAGE_S22 N       #> Shu et al. 2022 (CMAQ v5.3) Aerosol deposition model [default: N]

setenv BC_AERO_M2WET F       #> Specify whether or not boundary condition aerosol size distribution 
                             #>    is wet or dry [ default: F = dry ]. This option should be set
                             #>    to True if boundary condition size distirbution parameters are
                             #>    provided in terms of wet diameter (e.g. by an offline calculation,
                             #>    or a different 3D chemical transport model system).
setenv BC_AERO_M2USE T       #> Specify whether or not to use aerosol surface area from boundary 
                             #>    conditions [ default: T = use aerosol surface area  ]
                             #>    This setting can be significant for PM when using small domains.
                             #>    It is recommended to set this option to True if (1) using boundary
                             #>    conditions provided by a CMAQ simulation on a parent domain, (2) M2 
                             #>    is available, and (3) the domain is smaller than CONUS. 


#> Vertical Extraction Options
setenv VERTEXT N
setenv VERTEXT_COORD_PATH ${WORKDIR}/lonlat.csv

#> I/O Controls
setenv IOAPI_LOG_WRITE F     #> turn on excess WRITE3 logging [ options: T | F ]
setenv FL_ERR_STOP N         #> stop on inconsistent input files
setenv PROMPTFLAG F          #> turn on I/O-API PROMPT*FILE interactive mode [ options: T | F ]
setenv IOAPI_OFFSET_64 YES   #> support large timestep records (>2GB/timestep record) [ options: YES | NO ]
setenv IOAPI_CHECK_HEADERS N #> check file headers [ options: Y | N ]
setenv CTM_EMISCHK N         #> Abort CMAQ if missing surrogates from emissions Input files

#> Diagnostic Output Flags
setenv CTM_CKSUM Y           #> checksum report [ default: Y ]
setenv CLD_DIAG Y            #> cloud diagnostic file [ default: N ]

setenv CTM_PHOTDIAG Y        #> photolysis diagnostic file [ default: N ]
#setenv NLAYS_PHOTDIAG "1"    #> Number of layers for PHOTDIAG2 and PHOTDIAG3 from 
                             #>     Layer 1 to NLAYS_PHOTDIAG  [ default: all layers ] 
#setenv NWAVE_PHOTDIAG "294 303 310 316 333 381 607"  #> Wavelengths written for variables
                                                      #>   in PHOTDIAG2 and PHOTDIAG3 
                                                      #>   [ default: all wavelengths ]

setenv CTM_SSEMDIAG N        #> sea-spray emissions diagnostic file [ default: N ]
setenv CTM_DUSTEM_DIAG N     #> windblown dust emissions diagnostic file [ default: N ]; 
                             #>     Ignore if CTM_WB_DUST = N
setenv CTM_DEPV_FILE N       #> deposition velocities diagnostic file [ default: N ]
setenv VDIFF_DIAG_FILE N     #> vdiff & possibly aero grav. sedimentation diagnostic file [ default: N ]
setenv LTNGDIAG N            #> lightning diagnostic file [ default: N ]
setenv B3GTS_DIAG N          #> BEIS mass emissions diagnostic file [ default: N ]
setenv CTM_WVEL F            #> save derived vertical velocity component to conc 
                             #>    file [ default: Y ]

# =====================================================================
#> Input Directories and Filenames
# =====================================================================

set ICpath    = $INPDIR/icbc                        #> initial conditions input directory 
set BCpath    = $INPDIR/icbc                        #> boundary conditions input directory
set IN_LTpath = $INPDIR/lightning                   #> lightning NOx input directory
set METpath   = $INPDIR/met/mcipv5.4                #> meteorology input directory 
set JVALpath  = $INPDIR/jproc                      #> offline photolysis rate table directory
set JVALpath  = $OUTDIR                      #> offline photolysis rate table directory
set OMIpath   = $BLD                                #> ozone column data for the photolysis model
set EPICpath  = $INPDIR/epic                        #> EPIC putput for bidirectional NH3
set SZpath    = $INPDIR/surface                     #> surf zone file for in-line seaspray emissions

# =====================================================================
#> Begin Loop Through Simulation Days
# =====================================================================
set rtarray = ""

set TODAYG = ${START_DATE}
set TODAYJ = `date -ud "${START_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set START_DAY = ${TODAYJ} 
set STOP_DAY = `date -ud "${END_DATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set NDAYS = 0

while ($TODAYJ <= $STOP_DAY )  #>Compare dates in terms of YYYYJJJ
  
  set NDAYS = `echo "${NDAYS} + 1" | bc -l`

  #> Retrieve Calendar day Information
  set YYYYMMDD = `date -ud "${TODAYG}" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYMMDD
  set YYYYMM = `date -ud "${TODAYG}" +%Y%m`     #> Convert YYYY-MM-DD to YYYYMM
  set YYMMDD = `date -ud "${TODAYG}" +%y%m%d`   #> Convert YYYY-MM-DD to YYMMDD
  set MM = `date -ud "${TODAYG}" +%m`           #> Convert YYYY-MM-DD to MM  
  set YYYYJJJ = $TODAYJ

  #> Calculate Yesterday's Date
  set YESTERDAY = `date -ud "${TODAYG}-1days" +%Y%m%d` #> Convert YYYY-MM-DD to YYYYJJJ

# =====================================================================
#> Set Output String and Propagate Model Configuration Documentation
# =====================================================================
  echo ""
  echo "Set up input and output files for Day ${TODAYG}."

  #> set output file name extensions
  setenv CTM_APPL ${RUNID}_${YYYYMMDD} 
  
  #> Copy Model Configuration To Output Folder
  if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR
  cp $BLD/cbox_${VRSN}.cfg $OUTDIR/cbox_${CTM_APPL}.cfg

# =====================================================================
#> Input Files (Some are Day-Dependent)
# =====================================================================
  set ICBC_LAB = Empty

     setenv ICFILE Empty
     if ( ${MECH} =~ *cracmm* ) then
       setenv BOX_IC_FILE ${BLD}/cracmm1haps_NorthCarolina_Summer.txt
     else if ( ${MECH} =~ *cb6* ) then
       setenv BOX_IC_FILE ${BLD}/cb6r5hap_NorthCarolina_Summer.txt
     endif 
     setenv INIT_MEDC_1 notused

  #> Boundary conditions
  set BCFILE = Empty

  #> Off-line photolysis rates 
   set JVALfile  = JTABLE_${YYYYJJJ}

  #> Ozone column data
  set OMIfile   = omi_cmaq_2005through2024_27x27.dat

  #> Optics file
  set OPTfile = PHOT_OPTICS.dat

  #>Miscellaneous Box Parameters and Defaults
  setenv BOX_DATA ${BLD}/gc_chem_inputs.dat
  # metgeo input data
  setenv BOX_METGEO_FILE ${BLD}/metgeo_data.txt


  #> MCIP meteorology files 
  setenv GRID_BDY_2D Empty  # GRID files are static, not day-specific
  setenv GRID_CRO_2D Empty
  setenv GRID_CRO_3D Empty
  setenv GRID_DOT_2D Empty
  setenv MET_CRO_2D Empty
  setenv MET_CRO_3D Empty
  setenv MET_DOT_3D Empty
  setenv MET_BDY_3D Empty
  setenv LUFRAC_CRO Empty

  #> Control Files
  #>
  #> The CMAQ control files defined below are an integral part of controlling the behavior of the model simulation.
  #> Among other things, they control the variables output to ELMO files, the mapping of species in the emission 
  #> files to chemical species in the model, and other parameters configuring model input and output.
  #> Please carefully review the CMAQ chemical control file to ensure it is configured to be consistent with the 
  #> assumptions made when creating the emission files defined below and the desired chemical mechanism.
  #> For further information, please see:
  #> + CMAQ User's Guide Appendix F on 'ELMOv2':
  #>   https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixF_elmo_output.md
  #> + CMAQ User's Guide Appendix B on Emission Control using DESID:
  #>   https://github.com/USEPA/CMAQ/blob/main/DOCS/Users_Guide/Appendix/CMAQ_UG_appendixB_emissions_control.md
  #> 
  setenv CMAQ_CTRL_NML ${BLD}/CMAQ_Control.nml
  setenv CMAQ_CH_CTRL_NML ${BLD}/CMAQ_Chem_Control_${MECH}.nml

  #> The following namelist controls the mapping of meteorological land use types and the NH3 and Hg emission
  #> potentials
  setenv STAGECTRL_NML ${BLD}/CMAQ_Control_STAGE.nml
 
  #> Spatial Masks For Emissions Scaling
  #setenv CMAQ_MASKS $SZpath/OCEAN_${MM}_L3m_MC_CHL_chlor_a_12NE3.nc #> horizontal grid-dependent ocean file

  setenv N_EMIS_GR 0

  #> In-line point emissions configuration
  setenv N_EMIS_PT 0          #> Number of elevated source groups
 

  #> Inline lightning NOx configuration
  if ( $CTM_LNO_ONLINE == 'Y' ) then
  #> In-line lightning NOx options
     setenv USE_LTNG_DATA  Y        #> use hourly NLDN strike file [ default: Y ]
     if ( $USE_LTNG_DATA == Y ) then
        setenv LTNG_DATA ${IN_LTpath}/NLDN_12km_60min_${YYYYMMDD}.ioapi
	setenv LNO_OPTION 1 # default, use lightning strikes such as NLDN, WWLLNs
        # LNO_OPTION 2:  use GLM flashes
        # LNO_OPTION 3:  use GLM Energy
        # LNO_OPTION 4:  use synergized GLM/WWLLN Energy
	# LNO_OPTION 5:  use synergized GLM/WWLLNs Energy with ICCG adjustment to set upper bound

     endif
     setenv LTNGPARMS_FILE ${IN_LTpath}/LTNG_AllParms_12NE3.nc #> lightning parameter file
  endif

  if( $USE_SEGA_N == 'Y' ) then
     setenv SEGA_SOILINIT   $OUTDIR/CBOX_SSOILOUT_${RUNID}_${YESTERDAY}.nc
     if( $USE_SEGA_N_EF == 'Y') then
        setenv SEGA_EF ${INPDIR}/surface/BEIS4_SEGA_beld6_norm_emis_2018_12NE3.ncf
     endif
  endif

  #> In-line biogenic emissions configuration
  if ( $CTM_BIOGEMIS_BE == 'Y' ) then
     set IN_BEISpath = ${INPDIR}/surface
     setenv GSPRO          $BLD/gspro_biogenics.txt
     setenv BEIS_NORM_EMIS $IN_BEISpath/beis4_beld6_norm_emis.12NE3.nc
     if ($USE_SEGA_N == 'N') then
        setenv BEIS_SOILINP    $OUTDIR/CBOX_BSOILOUT_${RUNID}_${YESTERDAY}.nc
     endif
                             #> Biogenic NO soil input file; ignore if NEW_START = TRUE
  endif
  if ( $CTM_BIOGEMIS_MG == 'Y' ) then
         setenv MEGAN_SOILINP    $OUTDIR/CBOX_MSOILOUT_${RUNID}_${YESTERDAY}.nc
                             #> Biogenic NO soil input file; ignore if INITIAL_RUN = Y
                             #>                            ; ignore if IGNORE_SOILINP = Y
         setenv MEGAN_CTS $SZpath/megan3.2/CT3_nebench.ncf
         setenv MEGAN_EFS $SZpath/megan3.2/EF_nebench.ncf
         setenv MEGAN_LDF $SZpath/megan3.2/LDF_nebench.ncf
         if ($BDSNP_MEGAN == 'Y') then
            setenv BDSNPINP    $OUTDIR/CBOX_BDSNPOUT_${RUNID}_${YESTERDAY}.nc
            setenv BDSNP_FFILE $SZpath/megan3.2/FERT_nebench.ncf
            setenv BDSNP_NFILE $SZpath/megan3.2/NDEP_nebench.ncf
            setenv BDSNP_LFILE $SZpath/megan3.2/LANDTYPE_nebench.ncf
            setenv BDSNP_AFILE $SZpath/megan3.2/ARID_nebench.ncf
            setenv BDSNP_NAFILE $SZpath/megan3.2/NONARID_nebench.ncf
         endif
  endif

  #> In-line sea spray emissions configuration
  setenv OCEAN_1 Empty

  #> Bidirectional ammonia configuration
  if ( $CTM_ABFLUX == 'Y' ) then
     setenv E2C_SOIL ${EPICpath}/2018r1_EPIC0509_12NE3_soil.nc
     setenv E2C_CHEM ${EPICpath}/2018r1_EPIC0509_12NE3_time${YYYYMMDD}.nc
     setenv E2C_CHEM_YEST ${EPICpath}/2018r1_EPIC0509_12NE3_time${YESTERDAY}.nc
     setenv E2C_LU ${EPICpath}/beld4_12NE3_2011.nc
  endif

#> Inline Process Analysis 
  setenv CTM_PROCAN Y        #> use process analysis [ default: N]
  setenv PACM_INFILE ${NMLpath}/pa_${MECH}.ctl
  setenv PACM_REPORT $OUTDIR/"PA_REPORT".${YYYYMMDD}

#> Integrated Source Apportionment Method (ISAM) Options
 setenv CTM_ISAM N
 if ( $?CTM_ISAM ) then
    if ( $CTM_ISAM == 'Y' || $CTM_ISAM == 'T' ) then
       setenv SA_IOLIST ${WORKDIR}/isam_control.2018_12NE3.txt
       setenv ISAM_BLEV_ELEV " 1 1"
       setenv AISAM_BLEV_ELEV " 1 1"

       #> Set Up ISAM Initial Condition Flags
       if ($NEW_START == true || $NEW_START == TRUE ) then
          setenv ISAM_NEW_START Y
          setenv ISAM_PREVDAY
       else
          setenv ISAM_NEW_START N
          setenv ISAM_PREVDAY "$OUTDIR/CBOX_SA_CGRID_${RUNID}_${YESTERDAY}.nc"
       endif

       #> Set Up ISAM Output Filenames
       setenv SA_ACONC_1      "$OUTDIR/CBOX_SA_ACONC_${CTM_APPL}.csv"
       setenv SA_CONC_1       "$OUTDIR/CBOX_SA_CONC_${CTM_APPL}.csv"
       setenv SA_DD_1         "$OUTDIR/CBOX_SA_DRYDEP_${CTM_APPL}.csv"
       setenv SA_WD_1         "$OUTDIR/CBOX_SA_WETDEP_${CTM_APPL}.csv"
       setenv SA_CGRID_1      "$OUTDIR/CBOX_SA_CGRID_${CTM_APPL}.csv"

       #> Set optional ISAM regions files
       setenv ISAM_REGIONS $INPDIR/surface/GRIDMASK_STATES_12NE3.nc

       #> Options used to favor tracked species in reaction for Ozone-NOx chemistry
       setenv ISAM_O3_WEIGHTS 5   # weights for tracked species Default is 5
                                  #     OPTIONS
                                  # 1 does not weight any species
                                  # 2 weights NOx and subset of NOz species
                                  # 3 uses with from option 2 plus weight OVOC species, organic radicals and operators
                                  # 4 weight OVOC species, organic radicals and operators
                                  # 5 toggles between two weighting set based on VOC and NOx limited ozone production
       # Below options only used if ISAM_O3_WEIGHTS set to 5
       setenv ISAM_NOX_CASE  2    # weights for tracked species when ozone production is NOx limited. Default is 2
       setenv ISAM_VOC_CASE  4    # weights for tracked species when ozone production is VOC limited. Default is 4
       setenv VOC_NOX_TRANS  0.35 # value of Prod H2O2 over Prod HNO3 less than where
                                  # ISAM_VOC_CASE weights are used. Otherwise, ISAM_NOX_CASE
                                  # weights are used. Default is 0.35

    endif
 endif


#> Sulfur Tracking Model (STM)
 setenv STM_SO4TRACK Y        #> sulfur tracking [ default: N ]
 if ( $?STM_SO4TRACK ) then
    if ( $STM_SO4TRACK == 'Y' || $STM_SO4TRACK == 'T' ) then

      #> option to normalize sulfate tracers [ default: Y ]
      setenv STM_ADJSO4 Y

    endif
 endif

#> Decoupled Direct Method in 3D (DDM-3D) Options
 setenv CTM_DDM3D N    # Sets up requisite script settings for DDM-3D (default is N/F)
                       # Additionally requires for CCTM to be compiled for DDM-3D simulations

 set NPMAX    = 2      # Number of sensitivity parameters defined in SEN_INPUT
 setenv SEN_INPUT ${WORKDIR}/sensinput.2018_12NE3.dat

 setenv DDM3D_HIGH N   # allow higher-order sensitivity parameters in SEN_INPUT [ T | Y | F | N ] (default is N/F)

 if ($NEW_START == true || $NEW_START == TRUE ) then
    setenv DDM3D_RST N # begins from sensitivities from a restart file [ T | Y | F | N ] (default is Y/T)
    set S_ICpath =     # sensitivity fields are initialized to 0.0 on the first hour of the first day
    set S_ICfile =
 else
    setenv DDM3D_RST Y # begins from sensitivities from a restart file [ T | Y | F | N ] (default is Y/T)  
    set S_ICpath = $OUTDIR
    set S_ICfile = CBOX_SENGRID_${RUNID}_${YESTERDAY}.nc
 endif

 setenv CTM_NPMAX       $NPMAX
 setenv CTM_SENS_1      "$OUTDIR/CBOX_SENGRID_${CTM_APPL}.nc"
 setenv A_SENS_1        "$OUTDIR/CBOX_ASENS_${CTM_APPL}.nc"
 setenv CTM_SWETDEP_1   "$OUTDIR/CBOX_SENWDEP_${CTM_APPL}.nc"
 setenv CTM_SDRYDEP_1   "$OUTDIR/CBOX_SENDDEP_${CTM_APPL}.nc"
 setenv INIT_SENS_1     $S_ICpath/$S_ICfile
 
 
# =====================================================================
#> Output Files
# =====================================================================

  #> set output file names
  setenv S_CGRID         "$OUTDIR/CBOX_CGRID_${CTM_APPL}.csv"      #> 3D Inst. Concentrations
  setenv CTM_CONC_1      "$OUTDIR/CBOX_CONC_${CTM_APPL}.csv"       #> On-Hour Concentrations
  setenv A_CONC_1        "$OUTDIR/CBOX_ACONC_${CTM_APPL}.csv"      #> Hourly Avg. Concentrations
  setenv MEDIA_CONC      "$OUTDIR/CBOX_MEDIA_CONC_${CTM_APPL}.csv" #> NH3 Conc. in Media
  setenv CTM_DRY_DEP_1   "$OUTDIR/CBOX_DRYDEP_${CTM_APPL}.csv"    #> Hourly Dry Deposition
  setenv CTM_DEPV_DIAG   "$OUTDIR/CBOX_DEPV_${CTM_APPL}.csv"      #> Dry Deposition Velocities
  setenv B3GTS_S         "$OUTDIR/CBOX_B3GTS_S_${CTM_APPL}.csv"   #> Biogenic Emissions
  setenv SEGA_SOILOUT    "$OUTDIR/CBOX_SSOILOUT_${CTM_APPL}.csv"     #> Soil Emissions
  setenv BEIS_SOILOUT    "$OUTDIR/CBOX_BSOILOUT_${CTM_APPL}.csv"     #> Soil Emissions
  setenv MEGAN_SOILOUT   "$OUTDIR/CBOX_MSOILOUT_${CTM_APPL}.csv"     #> Soil Emissions
  setenv BDSNPOUT        "$OUTDIR/CBOX_BDSNPOUT_${CTM_APPL}.csv"     #> Soil Emissions
  setenv CTM_WET_DEP_1   "$OUTDIR/CBOX_WETDEP1_${CTM_APPL}.csv"   #> Wet Dep From All Clouds
  setenv CTM_WET_DEP_2   "$OUTDIR/CBOX_WETDEP2_${CTM_APPL}.csv"   #> Wet Dep From SubGrid Clouds
  setenv CTM_ELMO_1      "$OUTDIR/CCTM_ELMO_${CTM_APPL}.csv"      #> On-Hour Particle Diagnostics
  setenv CTM_AELMO_1     "$OUTDIR/CCTM_AELMO_${CTM_APPL}.csv"     #> Hourly Avg. Particle Diagnostics
  setenv CTM_RJ_1        "$OUTDIR/CBOX_PHOTDIAG1_${CTM_APPL}.csv" #> 2D Surface Summary from Inline Photolysis
  setenv CTM_RJ_2        "$OUTDIR/CBOX_PHOTDIAG2_${CTM_APPL}.csv" #> 3D Photolysis Rates 
  setenv CTM_RJ_3        "$OUTDIR/CBOX_PHOTDIAG3_${CTM_APPL}.csv" #> 3D Optical and Radiative Results from Photolysis
  setenv CTM_SSEMIS_1    "$OUTDIR/CBOX_SSEMIS_${CTM_APPL}.csv"    #> Sea Spray Emissions
  setenv CTM_DUST_EMIS_1 "$OUTDIR/CBOX_DUSTEMIS_${CTM_APPL}.csv"  #> Dust Emissions
  setenv CTM_BUDGET      "$OUTDIR/CBOX_BUDGET_${CTM_APPL}.txt"    #> Budget [Default Off]
  setenv CTM_IPR_1       "$OUTDIR/CBOX_PA_1_${CTM_APPL}.csv"      #> Process Analysis
  setenv CTM_IPR_2       "$OUTDIR/CBOX_PA_2_${CTM_APPL}.csv"      #> Process Analysis
  setenv CTM_IPR_3       "$OUTDIR/CBOX_PA_3_${CTM_APPL}.csv"      #> Process Analysis
  setenv CTM_IRR_1       "$OUTDIR/CBOX_IRR_1_${CTM_APPL}.csv"     #> Chem Process Analysis
  setenv CTM_IRR_2       "$OUTDIR/CBOX_IRR_2_${CTM_APPL}.csv"     #> Chem Process Analysis
  setenv CTM_IRR_3       "$OUTDIR/CBOX_IRR_3_${CTM_APPL}.csv"     #> Chem Process Analysis
  setenv CTM_DRY_DEP_MOS "$OUTDIR/CBOX_DDMOS_${CTM_APPL}.csv"     #> Dry Dep
  setenv CTM_DEPV_MOS    "$OUTDIR/CBOX_DEPVMOS_${CTM_APPL}.csv"   #> Dry Dep Velocity
  setenv CTM_VDIFF_DIAG  "$OUTDIR/CBOX_VDIFF_DIAG_${CTM_APPL}.csv" #> Vertical Dispersion Diagnostic
  setenv CTM_VSED_DIAG   "$OUTDIR/CBOX_VSED_DIAG_${CTM_APPL}.csv"  #> Particle Grav. Settling Velocity
  setenv CTM_LTNGDIAG_1  "$OUTDIR/CBOX_LNO3D_${CTM_APPL}.csv"      #> Hourly Avg Lightning NO
  setenv CTM_LTNGDIAG_2  "$OUTDIR/CBOX_LNO2DCOL_${CTM_APPL}.csv"   #> Column Total Lightning NO
  setenv CTM_VEXT_1      "$OUTDIR/CBOX_VEXT_${CTM_APPL}.csv"       #> On-Hour 3D Concs at select sites

  #> set floor file (neg concs)
  setenv FLOOR_FILE ${OUTDIR}/FLOOR_${CTM_APPL}.txt

  #> look for existing log files and output files
  ( ls CTM_LOG_???.${CTM_APPL} > buff.txt ) >& /dev/null
  ( ls ${LOGDIR}/CTM_LOG_???.${CTM_APPL} >> buff.txt ) >& /dev/null
  set log_test = `cat buff.txt`; rm -f buff.txt

  set OUT_FILES = (${FLOOR_FILE} ${S_CGRID} ${CTM_CONC_1} ${A_CONC_1} ${MEDIA_CONC}         \
             ${CTM_DRY_DEP_1} $CTM_DEPV_DIAG $B3GTS_S $MEGAN_SOILOUT $BEIS_SOILOUT $BDSNPOUT \
             $SEGA_SOILOUT $CTM_WET_DEP_1 $CTM_WET_DEP_2 $CTM_ELMO_1 $CTM_AELMO_1             \
             $CTM_RJ_1 $CTM_RJ_2 $CTM_RJ_3 $CTM_SSEMIS_1 $CTM_DUST_EMIS_1 $CTM_IPR_1 $CTM_IPR_2       \
             $CTM_IPR_3 $CTM_BUDGET $CTM_IRR_1 $CTM_IRR_2 $CTM_IRR_3 $CTM_DRY_DEP_MOS                 \
             $CTM_DEPV_MOS $CTM_VDIFF_DIAG $CTM_VSED_DIAG $CTM_LTNGDIAG_1 $CTM_LTNGDIAG_2 $CTM_VEXT_1 )
  if ( $?CTM_ISAM ) then
     if ( $CTM_ISAM == 'Y' || $CTM_ISAM == 'T' ) then
        set OUT_FILES = (${OUT_FILES} ${SA_ACONC_1} ${SA_CONC_1} ${SA_DD_1} ${SA_WD_1}      \
                         ${SA_CGRID_1} )
     endif
  endif
  if ( $?CTM_DDM3D ) then
     if ( $CTM_DDM3D == 'Y' || $CTM_DDM3D == 'T' ) then
        set OUT_FILES = (${OUT_FILES} ${CTM_SENS_1} ${A_SENS_1} ${CTM_SWETDEP_1} ${CTM_SDRYDEP_1} )
     endif
  endif
  set OUT_FILES = `echo $OUT_FILES | sed "s; -v;;g" | sed "s;MPI:;;g" `
  ( ls $OUT_FILES > buff.txt ) >& /dev/null
  set out_test = `cat buff.txt`; rm -f buff.txt
  
  #> delete previous output if requested
  if ( $CLOBBER_DATA == true || $CLOBBER_DATA == TRUE  ) then
     echo 
     echo "Existing Logs and Output Files for Day ${TODAYG} Will Be Deleted"

     #> remove previous log files
     foreach file ( ${log_test} )
        #echo "Deleting log file: $file"
        /bin/rm -f $file  
     end
 
     #> remove previous output files
     foreach file ( ${out_test} )
        #echo "Deleting output file: $file"
        /bin/rm -f $file  
     end
     /bin/rm -f ${OUTDIR}/C*DESID*${CTM_APPL}.csv ${OUTDIR}/C*ELMO*${CTM_APPL}.csv

  else
     #> error if previous log files exist
     if ( "$log_test" != "" ) then
       echo "*** Logs exist - run ABORTED ***"
       echo "*** To overide, set CLOBBER_DATA = TRUE in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif
     
     #> error if previous output files exist
     if ( "$out_test" != "" ) then
       echo "*** Output Files Exist - run will be ABORTED ***"
       foreach file ( $out_test )
          echo " cannot delete $file"
       end
       echo "*** To overide, set CLOBBER_DATA = TRUE in run_cctm.csh ***"
       echo "*** and these files will be automatically deleted. ***"
       exit 1
     endif
  endif

  #> for the run control ...
  setenv CTM_STDATE      $YYYYJJJ
  setenv CTM_STTIME      $STTIME
  setenv CTM_RUNLEN      $NSTEPS
  setenv CTM_TSTEP       $TSTEP
  setenv INIT_CONC_1 $ICpath/$ICFILE
# if( ! ( -e ${INIT_CONC_1} ) )then
#    ls ${INIT_CONC_1}
#    exit()
# endif
  setenv BNDY_CONC_1 $BCpath/$BCFILE
  setenv OMI $OMIpath/$OMIfile
  setenv MIE_TABLE $OUTDIR/mie_table_coeffs_${compilerString}.txt
  setenv OPTICS_DATA $OMIpath/$OPTfile
  setenv XJ_DATA $JVALpath/$JVALfile
  if( ${CLOBBER_DATA} != FALSE || ${CLOBBER_DATA} != false ) \rm -f ${XJ_DATA}
 
  #> species defn & photolysis
  setenv gc_matrix_nml ${NMLpath}/GC_$MECH.nml
  setenv ae_matrix_nml ${NMLpath}/AE_$MECH.nml
  setenv nr_matrix_nml ${NMLpath}/NR_$MECH.nml
  setenv tr_matrix_nml ${NMLpath}/Species_Table_TR_0.nml
 
  #> check for photolysis input data
  setenv CSQY_DATA ${NMLpath}/CSQY_DATA_$MECH

  if (! (-e $CSQY_DATA ) ) then
     echo " $CSQY_DATA  not found "
     exit 1
  endif
  if (! (-e $OPTICS_DATA ) ) then
     echo " $OPTICS_DATA  not found "
#    exit 1
  endif

# =====================================================================
#> Photolysis Data Input Directories
# =====================================================================

 set PHOT_DATA  = ${CMAQ_REPO}/UTIL/inline_phot_preproc/photolysis_CSQY_data
 set CSQYpath   = $PHOT_DATA # CSQY input data
 set PROFpath   = $PHOT_DATA # PROF input data
 set ETpath     = $PHOT_DATA # ET input data
 set TOMSpath   = $PHOT_DATA # TOMS input data

# =====================================================================
#> Photolysis Input Files
# =====================================================================

 set ETfile    = ETirradiance.dat
 set PROFfile  = PROFILES.dat
 set O2ABSfile = O2_JPL06-2
 set O3ABSfile = O3O1D_JPL06-2
 set TOMSfile  = not_available

#>- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 setenv ET        $ETpath/$ETfile
 setenv PROFILES  $PROFpath/$PROFfile
 setenv TOMS      $TOMSpath/$TOMSfile
 setenv O2ABS     $CSQYpath/$O2ABSfile
 setenv O3ABS     $CSQYpath/$O3ABSfile
 setenv CSQY      $CSQYpath

# ===================================================================
#> Execution Portion
# ===================================================================

  #> Print attributes of the executable
  if ( $CTM_DIAG_LVL != 0 ) then
     ls -l $BLD/$EXEC
     size $BLD/$EXEC
     unlimit
     limit
  endif

  #> Print Startup Dialogue Information to Standard Out
  echo 
  echo "CMAQ Processing of Day $YYYYMMDD Began at `date`"
  echo 

if ( ! ( -e $BLD/$EXEC ) ) then
   ls $BLD/$EXEC
   exit()
endif

if ( $PROC == serial ) then

  #> Executable call for single PE, uncomment to invoke
   ( /usr/bin/time -p $BLD/$EXEC ) |& tee buff_${EXECUTION_ID}.txt
else
  #> Executable call for multi PE, configure for your system 
  # set MPI = /usr/local/intel/impi/3.2.2.006/bin64
  # set MPIRUN = $MPI/mpirun
  ( /usr/bin/time -p mpirun -np $NPROCS $BLD/$EXEC ) |& tee buff_${EXECUTION_ID}.txt
endif
  
  #> Harvest Timing Output so that it may be reported below
  set rtarray = "${rtarray} `tail -3 buff_${EXECUTION_ID}.txt | grep -Eo '[+-]?[0-9]+([.][0-9]+)?' | head -1` "
  rm -rf buff_${EXECUTION_ID}.txt

  #> Abort script if abnormal termination
  if ( ! -e $OUTDIR/CBOX_CGRID_${CTM_APPL}.csv ) then
    echo ""
    echo "**************************************************************"
    echo "** Runscript Detected an Error: CGRID file was not written. **"
    echo "**   This indicates that CMAQ was interrupted or an issue   **"
    echo "**   exists with writing output. The runscript will now     **"
    echo "**   abort rather than proceeding to subsequent days.       **"
    echo "**************************************************************"
    break
  endif

  #> Print Concluding Text
  echo 
  echo "CMAQ Processing of Day $YYYYMMDD Finished at `date`"
  echo
  echo "\\\\\=====\\\\\=====\\\\\=====\\\\\=====/////=====/////=====/////=====/////"
  echo

# ===================================================================
#> Finalize Run for This Day and Loop to Next Day
# ===================================================================

  #> Save Log Files and Move on to Next Simulation Day
# mv CTM_LOG_???.${CTM_APPL} $LOGDIR
  if ( $CTM_DIAG_LVL != 0 ) then
    mv CTM_DIAG_???.${CTM_APPL} $LOGDIR
  endif

  #> The next simulation day will, by definition, be a restart
  setenv NEW_START false

  #> Increment both Gregorian and Julian Days
  set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
  set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

end  #Loop to the next Simulation Day

# ===================================================================
#> Generate Timing Report
# ===================================================================
set RTMTOT = 0
foreach it ( `seq ${NDAYS}` )
    set rt = `echo ${rtarray} | cut -d' ' -f${it}`
    set RTMTOT = `echo "${RTMTOT} + ${rt}" | bc -l`
end

set RTMAVG = `echo "scale=2; ${RTMTOT} / ${NDAYS}" | bc -l`
set RTMTOT = `echo "scale=2; ${RTMTOT} / 1" | bc -l`

echo
echo "=================================="
echo "  ***** CMAQ TIMING REPORT *****"
echo "=================================="
echo "Start Day: ${START_DATE}"
echo "End Day:   ${END_DATE}"
echo "Number of Simulation Days: ${NDAYS}"
echo "Domain Name:               ${GRID_NAME}"
echo "Number of Grid Cells:      ${NCELLS}  (ROW x COL x LAY)"
echo "Number of Layers:          ${NZ}"
echo "Number of Processes:       ${NPROCS}"
echo "   All times are in seconds."
echo
echo "Num  Day        Wall Time"
set d = 0
set day = ${START_DATE}
foreach it ( `seq ${NDAYS}` )
    # Set the right day and format it
    set d = `echo "${d} + 1"  | bc -l`
    set n = `printf "%02d" ${d}`

    # Choose the correct time variables
    set rt = `echo ${rtarray} | cut -d' ' -f${it}`

    # Write out row of timing data
    echo "${n}   ${day}   ${rt}"

    # Increment day for next loop
    set day = `date -ud "${day}+1days" +%Y-%m-%d`
end
echo "     Total Time = ${RTMTOT}"
echo "      Avg. Time = ${RTMAVG}"

exit
