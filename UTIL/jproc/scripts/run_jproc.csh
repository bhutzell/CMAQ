#!/bin/csh -f

# ====================== JPROCv6.X Run Script ======================= 
# Usage: run_jproc.csh >&! jproc.log &                                 
#
# To report problems or request help with this script/program:        
#             http://www.cmascenter.org
# =================================================================== 

 if ( $#argv == 1 ) then
    setenv compiler $argv[1]
    setenv compilerVrsn Empty
 else if ( $#argv == 2 ) then
    #> Compiler Name and Version have been provided
    setenv compiler $1
    setenv compilerVrsn $2
 else
    echo "usage: $0 <compiler>"
    echo "using default compiler: intel"
    setenv compiler intel 
    setenv compilerVrsn Empty
 endif

#> Source the config.cmaq file.csh to set the build environment
 source ../../../config_cmaq.csh


#> Check that CMAQ_REPO is set: 
 if ( ! -e $CMAQ_REPO ) then
    echo "   $CMAQ_REPO path does not exist"
    exit 1
    endif
 echo " "; echo " Input data path, CMAQ_REPO set to $CMAQ_REPO"; echo " "


 set VRSN     = v6 
 set MECH     = cb6r5_ae7_aq 
 #set MECH     = cracmm3
 set APPL     = ${VRSN}_${MECH}
 set EXEC     = JPROC_${APPL}_${compiler}${compilerVrsn}    #> executable name
 set CFG      = cfg.$EXEC                                   #> configuration file name


#> Set the working directory:
 set BASE  = $cwd
 set BLD   = ${BASE}/BLD_${APPL}_${compiler}${compilerVrsn}

 cd $BASE; date; set timestamp; cat $BASE/cfg.${CFG}; echo " "; set echo

#> JPROC run dates (produces one file per day)

 set STDATE   = 20180701  #> the beginning day for this run
 set ENDATE   = 20180702  #> the ending day

# =====================================================================
#> Input/Output Directories
# =====================================================================

 set CMAQ_DATA  = ${CMAQ_REPO}/UTIL/inline_phot_preproc/photolysis_CSQY_data
 set CSQYpath   = $CMAQ_DATA # CSQY input data
 set PROFpath   = $CMAQ_DATA # PROF input data
 set ETpath     = $CMAQ_DATA # ET input data
 set TOMSpath   = $CMAQ_DATA # TOMS input data
 set OUTDIR     = $BASE/jtable_${APPL}     # Output directory

# =====================================================================
#> Input Files
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

# check ET input file

 if (! ( -e $ET ) ) then
    echo " $ET not found "
    exit
 endif

# check profile input file

 if (! ( -e $PROFILES ) ) then
    echo " $PROFILES not found "
    exit
 endif

# check TOMS input file

 setenv JPROC_TOMSEXIST  N  # Assume TOMS data file does not exist for this run
 if ( -e $TOMS ) then
    setenv JPROC_TOMSEXIST  Y
 endif

# check O2 absorption input file

 if (! ( -e $O2ABS ) ) then
    echo " $O2ABS not found "
    exit
 endif

# check O3 absorption input file

 if (! ( -e $O3ABS ) ) then
    echo " $O3ABS not found "
    exit
 endif

 if ( ! -d "$OUTDIR" ) mkdir -p $OUTDIR

 ls -l $BLD/$EXEC
 unlimit
 limit

 unalias rm
 
set TODAYG = ${STDATE}
set TODAYJ = `date -ud "${STDATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ
set START_DAY = ${TODAYJ}
set STOP_DAY = `date -ud "${ENDATE}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

 while ( $TODAYJ <= $STOP_DAY )         # Loop thru all the days to run

    setenv JPROC_STDATE $TODAYJ
    echo "   Running for $TODAYJ ..."
    set JVfile = JTABLE_${TODAYJ}     # Daily output file name
    setenv JVALUES $OUTDIR/$JVfile
    if ( -e $JVALUES ) rm $JVALUES  # Remove existing output file

#   Executable call:
    time $BLD/$EXEC
    if ( ${status} )then
       echo "runtime ERROR executing ${BLD}/${EXEC}"
    else
       echo "created ${JVfile}"
    endif

    set TODAYG = `date -ud "${TODAYG}+1days" +%Y-%m-%d` #> Add a day for tomorrow
    set TODAYJ = `date -ud "${TODAYG}" +%Y%j` #> Convert YYYY-MM-DD to YYYYJJJ

 end

 exit() 
