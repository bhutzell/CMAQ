#! /bin/csh -f

# ==================== WRITESITE_v5.5.X Run Script ====================
# Usage: run_writesite.csh >&! writesite.log &
#
# To report problems or request help with this script/program:
#             http://www.epa.gov/cmaq    (EPA CMAQ Website)
#             http://www.cmascenter.org
# ===================================================================

# ==================================================================
#> Runtime Environment Options
# ==================================================================

#> Choose compiler and set up CMAQ environment with correct 
#> libraries using config.cmaq. Options: intel | gcc | pgi
 setenv compiler intel 
#setenv compiler gcc

 cd ../../..
 source ./config_cmaq.csh

#> Set General Parameters for Configuring the Simulation
 set VRSN      = v55               #> Code Version
 set PROC      = mpi               #> serial or mpi
 set MECH      = cracmm3           #> Mechanism ID
 set APPL      = Bench_2018_12NE3  #> Application Name (e.g. Gridname)
                                                      
#> Define RUNID as any combination of parameters above or others. By default,
#> this information will be collected into this one string, $RUNID, for easy
#> referencing in output binaries and log files as well as in other scripts.
 set RUNID = ${VRSN}_${compilerString}_${APPL}

#> Set the build directory if this was not set above 
#> (this is where the executable is located by default).
 if ( ! $?BINDIR ) then
  set BINDIR = ${CMAQ_HOME}/POST/writesite/scripts/BLD_writesite_${VRSN}_${compilerString}
 endif
set echo

#> Set the name of the executable.
 set EXEC = writesite_${VRSN}.exe

#> Set location of CMAQ repo.  This will be used to point to the optional time zone file
#> used by writesite. 
 set REPO_HOME = ${CMAQ_REPO}

#> Set output directory
 set POSTDIR = ${CMAQ_DATA}/POST     #> Location where writesite file will be written

  if ( ! -e $POSTDIR ) then
    mkdir $POSTDIR
  endif

# =====================================================================
#> WRITESITE Configuration Options
# =====================================================================

#> Projection sphere type used by I/OAPI (use type #20 to match WRF/CMAQ)
 setenv IOAPI_ISPH 20

#> name of input file containing sites to process (default is all cells)
#setenv SITE_FILE ALL
#> Sample SITE_FILE text file is available in the v5.2.1 repo.
 setenv SITE_FILE ${REPO_HOME}/POST/writesite/inputs/sites.txt
 setenv SITE_FILE /work/MOD3DEV/hwo/cmaq_testbed/boxmodel_Feb-09-2026_parallel/POST/writesite//inputs/sites_washdc.txt
 setenv SITE_FILE /work/MOD3DEV/hwo/cmaq_testbed/boxmodel_Feb-09-2026_parallel/POST/writesite//inputs/sites_NewYork.txt

#> delimiter used in site file (default is <tab>)
 setenv DELIMITER ','

#> site file contains column/row values (default is N, meaning lon/lat values will be used)
 setenv USECOLROW N

#> location of time zone data file, tz.csv (this is a required input file)
#> The tz.csv file is saved within the bldoverlay folder of the v5.2.1 repo which also uses this input.
 setenv TZFILE ${REPO_HOME}/POST/writesite/inputs/tz.csv

#> grid layer to output (default is 1)
 setenv LAYER 1

#> adjust to local standard time (default is N)
 setenv USELOCAL N

#> shifts time of data (default is 0)
#setenv TIME_SHIFT 1

#> output header records (default is Yes)
 setenv PRTHEAD  Y

#> output map projection coordinates x and y (default is Yes)
 setenv PRT_XY   N         

#>write put table that pivot variables and date-time to rows and columns, respectively
#>PIVOT_TABLE more supports create initial concentrations file for box model (CBOX)
#>only available if SITE_FILE does not equal ALL. (Default is N)
 setenv PIVOT_TABLE Y 

#> define time window
 set START_DATE = "2018-07-03"     #> first date to process (default is starting date of input file)
 set END_DATE   = "2018-07-03"     #> last date to process (default is ending date of input file)
 setenv START_TIME 00000     #> first time to process (default is starting time of input file)
 setenv END_TIME   24000     #> last time to process (default is 240000)

#> Convert START_DATE and END_DATE to Julian day.
#> (required format for writesite STARTDATE and ENDDATE environment variables)
 if( $?STARTDATE )then
    setenv STARTDATE `date -ud "${START_DATE}" +%Y%j`
    setenv ENDDATE   `date -ud "${END_DATE}" +%Y%j`
    setenv YYYYMMDD  `date -ud "${START_DATE}" +%Y%m%d`
 else
    setenv YYYYMMDD "WriteSite"
 endif

#> list of species to output
 setenv SPECIES_1 ALL

#> set input and output files
 setenv INFILE  /work/MOD3DEV/hwo/cmaq_testbed/boxmodel_Feb-17-2026_parallel/data/output_CCTM_v60_cracmm3m_intel23.2_Bench_2018_12NE3/CCTM_CGRID_v60_cracmm3m_intel23.2_Bench_2018_12NE3_20180702.nc
#setenv INFILE  /work/MOD3DEV/hwo/cmaq_testbed/boxmodel_Feb-17-2026_parallel/data/output_CCTM_v60_cb6r5m_ae7_aq_intel23.2_Bench_2018_12NE3/CCTM_CGRID_v60_cb6r5m_ae7_aq_intel23.2_Bench_2018_12NE3_20180702.nc
        #[Add location of input file, e.g. COMBINE_ACONC file.]
 setenv OUTFILE    ${POSTDIR}/cgrid_${RUNID}_${YYYYMMDD}.txt
 setenv PIVOT_FILE ${POSTDIR}/cgrid_${RUNID}_${YYYYMMDD}_pivot.txt
 echo $APPL
 echo $RUNID
 echo $YYYYMMDD
 echo ${PIVOT_FILE}

#> Executable call:
 ${BINDIR}/${EXEC}

 set progstat = ${status}
 if ( ${progstat} ) then
   echo "ERROR ${progstat} in $BINDIR/$EXEC"
   exit( ${progstat} )
 endif

 date
 exit()



